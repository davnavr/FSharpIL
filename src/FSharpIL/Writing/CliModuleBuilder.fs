namespace FSharpIL.Writing

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

open FSharpIL.Writing.BuildErrors
open FSharpIL.Writing.Cil

open FSharpIL.Utilities
open FSharpIL.Utilities.Collections
open FSharpIL.Utilities.Compare

type EntryPoint =
    private
    | NoEntryPoint
    | EntryPointMethod of MethodTok<DefinedType, MethodDefinition<MethodKinds.Static>>

[<RequireQualifiedAccess>]
module CustomAttribute =
    /// Returns the number of fixed arguments from the specified constructor method.
    type Resolver = CustomAttributeCtor -> ValidationResult<int32>

    [<IsReadOnly; Struct>]
    [<NoComparison; NoEquality>]
    type Constructed = { Constructor: CustomAttributeCtor; Signature: CustomAttrib }

    [<IsReadOnly; Struct>]
    [<NoComparison; CustomEquality>]
    type Owner =
        { Owner: obj }
        interface IEquatable<Owner> with member this.Equals other = this.Owner.Equals other.Owner

        static member DefinedAssembly(assembly: DefinedAssembly) = { Owner = assembly }
        static member DefinedType(tdef: DefinedType) = { Owner = tdef }
        static member DefinedField(field: Field) = { Owner = field }
        static member DefinedMethod(method: DefinedMethod) = { Owner = method }

    let rec private createFixedArgsLoop
        (args: byref<FixedArg[]>)
        (parameterTypes: ImmutableArray<ParameterType>)
        (parameterNames: ImmutableArray<Parameter>)
        (source: FixedArgSource)
        i
        =
        if i < args.Length then
            let paramName =
                if parameterNames.IsDefaultOrEmpty
                then ValueNone
                else parameterNames.ItemRef(i).ParamName

            match parameterTypes.[i] with
            | ParameterType.T t ->
                match CliType.toElemType t with
                | ValueSome etype ->
                    match source i paramName etype with
                    | Ok fixedArg ->
                        args.[i] <- fixedArg
                        createFixedArgsLoop &args parameterTypes parameterNames source (i + 1)
                    | Error(ValueSome err) -> Error err
                    | Error ValueNone -> Error(noImpl "error for no argument provided to custom attribute constructor")
                | ValueNone -> Error(noImpl "error for invalid custom attribute argument type")
            | _ -> Error(noImpl "error for custom attribute argument type cannot be byref type")
        else Ok(Unsafe.As<_, ImmutableArray<FixedArg>> &args)

    let private createFixedArgs source numFixedArgs (customAttributeCtor: CustomAttributeCtor) =
        let mutable fixedArgs = Array.zeroCreate numFixedArgs

        let parameterNames =
            match customAttributeCtor.Constructor.Member with
            | :? DefinedMethod as mdef -> mdef.Parameters
            | _ -> ImmutableArray.Empty

        createFixedArgsLoop &fixedArgs customAttributeCtor.Constructor.Member.ParameterTypes parameterNames source 0

    [<Sealed>]
    type LookupBuilder (resolver: Resolver) =
        let lookup = Dictionary<Owner, ImmutableArray<_>.Builder>()

        member _.OwnerCount = lookup.Count

        member _.GetEnumerator() = lookup.GetEnumerator()

        member _.GetAttributeCount owner =
            match lookup.TryGetValue owner with
            | true, attributes -> attributes.Count
            | false, _ -> 0

        member _.Add(owner, attrib) =
            let attributes =
                match lookup.TryGetValue owner with
                | true, existing -> existing
                | false, _ ->
                    let attributes = ImmutableArray.CreateBuilder<Constructed>()
                    lookup.[owner] <- attributes
                    attributes
            attributes.Add attrib

        member this.TryAdd(owner, attrib: CustomAttribute) =
            canfail {
                let! numFixedArgs = resolver attrib.Constructor
                let! fixedArguments = createFixedArgs attrib.FixedArguments numFixedArgs attrib.Constructor
                let blob = { FixedArgs = fixedArguments; NamedArgs = attrib.NamedArguments }
                this.Add(owner, { Constructed.Constructor = attrib.Constructor; Signature = blob })
            }

[<IsReadOnly; Struct>]
[<NoComparison; NoEquality>]
type CustomAttributeList (owner: CustomAttribute.Owner, lookup: CustomAttribute.LookupBuilder) =
    member _.Count = lookup.GetAttributeCount owner
    member _.Add attrib = lookup.TryAdd(owner, attrib)

[<RequireQualifiedAccess>]
module CustomAttributeList =
    let inline fromRef owner lookup attributes =
        match attributes with
        | ValueSome attributes' -> attributes' := CustomAttributeList(owner, lookup)
        | ValueNone -> ()

type CustomAttributeBuilder = CustomAttributeList ref voption

[<Sealed>]
type DefinedTypeMembers (owner: DefinedType, warnings: _ option, methodBodyCache: MethodBodyStream, namedTypeCache, entryPointToken, attrs) =
    [<DefaultValue>] val mutable internal Field: HybridHashSet<DefinedField>
    [<DefaultValue>] val mutable Method: HybridHashSet<DefinedMethod>
    [<DefaultValue>] val mutable internal MethodBodyLookup: LateInitDictionary<DefinedMethod, MethodBody>

    member _.Owner = owner
    member this.FieldCount = this.Field.Count
    member this.MethodCount = this.Method.Count

    member this.AddDefinedField field =
        if this.Field.Add field
        then Ok(FieldTok.ofTypeDef owner field namedTypeCache)
        else ValidationResult.Error(noImpl "error for duplicate field")

    member this.AddDefinedField(field: 'Field, attributes) =
        validated {
            let! token = this.AddDefinedField field
            CustomAttributeList.fromRef (CustomAttribute.Owner.DefinedField field) attrs attributes
            return token
        }

    member this.DefineField(field: DefinedField, attributes) = this.AddDefinedField(field, attributes)

    member private this.AddDefinedMethod(method, body: MethodBody voption) =
        // TODO: Check that owner is the correct kind of type to own this kind of method.
        if this.Method.Add method then
            match body with
            | ValueSome body' ->
                methodBodyCache.Add body' |> ignore
                this.MethodBodyLookup.Inner.Add(method, body')
            | ValueNone -> ()

            Ok(MethodTok.ofTypeDef owner method namedTypeCache)
        else ValidationResult.Error(noImpl "error for duplicate method")

    member this.AddDefinedMethod(method: 'Method, body, attributes) =
        validated {
            let! target = this.AddDefinedMethod(method, body)
            CustomAttributeList.fromRef (CustomAttribute.Owner.DefinedMethod method) attrs attributes
            return target
        }

    member this.DefineMethod(method: DefinedMethod, body, attributes) = this.AddDefinedMethod<_>(method, body, attributes)

    member this.DefineEntryPoint(method: EntryPointMethod, body, attributes) =
        validated {
            match owner with
            | DefinedType.Generic _ ->
                return! ValidationResult.failure
                    { GenericTypeCannotHaveEntryPoint.Owner = owner
                      GenericTypeCannotHaveEntryPoint.Method = method }
            | DefinedType.Definition _ -> ()

            let! target = this.AddDefinedMethod(method.Method, ValueSome body, attributes)
            entryPointToken := EntryPoint.EntryPointMethod target
            return target
        }

    member this.ContainsField field = this.Field.Contains field
    member this.ContainsMethod method = this.Method.Contains method

[<Sealed>]
type ReferencedTypeMembers (owner: ReferencedType, warnings: ValidationWarningsBuilder option, namedTypeCache) =
    [<DefaultValue>] val mutable Field: HybridHashSet<ReferencedField>
    [<DefaultValue>] val mutable Method: HybridHashSet<ReferencedMethod>

    member _.Owner = owner
    member this.FieldCount = this.Field.Count
    member this.MethodCount = this.Method.Count

    member this.AddReferencedMethod(method: 'Method when 'Method :> ReferencedMethod) =
        // TODO: Check that the kind and owner of method are correct.
        if this.Method.Add method
        then ValidationResult<MethodTok<_, 'Method>>.Ok(MethodTok.ofTypeRef owner method namedTypeCache)
        else Error(noImpl "error for duplicate method")

    member this.ReferenceMethod(method: ReferencedMethod) = this.AddReferencedMethod method

    member this.ContainsField field = this.Field.Contains field
    member this.ContainsMethod method = this.Method.Contains method

[<Sealed>]
type MemberIndexCounter<'Row when 'Row :> ITableRow> () =
    let mutable last, first = 0u, ValueNone

    member _.Last
        with get(): TableIndex<'Row> =
            match first with
            | ValueSome _ -> { TableIndex = last }
            | ValueNone -> { TableIndex = last + 1u }
        and set({ TableIndex = index }: TableIndex<'Row>) =
            last <- index

            match first with
            | ValueNone -> first <- ValueSome last
            | ValueSome _ -> ()

    member _.First: TableIndex<'Row> =
        match first with
        | ValueSome i -> { TableIndex = i }
        | ValueNone -> { TableIndex = last + 1u }

    member _.Reset() = first <- ValueNone

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type GenericParamEntry =
    { ParameterIndex: TableIndex<GenericParamRow>
      Parameters: ImmutableArray<GenericParam>
      ConstraintIndex: TableIndex<GenericParamConstraintRow> }

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type TypeSpecMember<'Member when 'Member : not struct> = { Owner: CliType; Member: 'Member }

[<NoComparison; NoEquality>]
type ModuleBuilderInfo =
    { Header: CliHeader
      Root: CliMetadataRoot<FSharpIL.Omitted, FSharpIL.Omitted>
      Name: Identifier
      Mvid: Guid
      [<DefaultValue>] mutable GlobalMembers: DefinedTypeMembers
      NamedTypes: NamedTypeCache
      CustomAttributes: CustomAttribute.LookupBuilder
      mutable Assembly: DefinedAssembly option
      AssemblyReferences: HashSet<ReferencedAssembly>
      DefinedTypes: Dictionary<DefinedType, DefinedTypeMembers>
      NestedTypes: Dictionary<DefinedType, List<DefinedType>>
      NonNestedTypes: List<DefinedType>
      ReferencedTypes: Dictionary<ReferencedType, ReferencedTypeMembers>
      FieldReferences: ImmutableArray<TypeSpecMember<Field>>.Builder // TODO: Rename to GenericTypeFields
      MethodReferences: ImmutableArray<TypeSpecMember<Method>>.Builder // TODO: Rename to GenericTypeMethods
      //MethodSpecifications: 
      MethodBodies: MethodBodyStream
      EntryPoint: EntryPoint ref }

[<IsReadOnly; Struct>]
type DefinedTypeMembers<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct> =
    val Members: DefinedTypeMembers

    new (members) = { Members = members }

    member inline this.DefineMethod(method: 'Method, body, attributes)  =
        match this.Members.AddDefinedMethod(method, body, attributes) with
        | Ok token -> ValidationResult<MethodTok<TypeDefinition<'Kind>, 'Method>>.Ok(MethodTok.unsafeAs token.Token)
        | Error err -> Error err

[<IsReadOnly; Struct>]
type ReferencedTypeMembers<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct> =
    val Members: ReferencedTypeMembers

    new (members) = { Members = members }

    member inline this.ReferenceMethod(method: 'Method): ValidationResult<MethodTok<TypeReference<'Kind>, 'Method>> =
        match this.Members.AddReferencedMethod method with
        | Ok token -> Ok(MethodTok.unsafeAs token.Token)
        | Error err -> Error err

[<AbstractClass; Sealed>]
type TypeMemberExtensions = // TODO: For these extension methods, call internal version that skips some validation.
    static member ReferenceMethod(members: ReferencedTypeMembers<'Kind> when 'Kind :> TypeAttributes.IHasStaticMethods, method) =
        members.ReferenceMethod<MethodReference<MethodKinds.ObjectConstructor>> method

    static member ReferenceMethod(members: ReferencedTypeMembers<'Kind> when 'Kind :> TypeAttributes.IHasStaticMethods, method) =
        members.ReferenceMethod<MethodReference<MethodKinds.Static>> method

    static member DefineMethod
        (
            members: DefinedTypeMembers<'Kind> when 'Kind :> TypeKinds.IHasConstructors,
            method,
            body,
            attributes
        )
        =
        members.DefineMethod<MethodDefinition<MethodKinds.ObjectConstructor>>(method, ValueSome body, attributes)

    static member DefineMethod
        (
            members: DefinedTypeMembers<'Kind> when 'Kind :> TypeKinds.IHasInstanceMethods,
            method,
            body,
            attributes
        )
        =
        members.DefineMethod<MethodDefinition<MethodKinds.Instance>>(method, ValueSome body, attributes)

    //static member DefineEntryPoint(members: ReferenceTypeMembers<'Kind> 

[<Sealed>]
type CliModuleBuilder // TODO: Consider making an immutable version of this class.
    (
        name,
        ?mvid,
        ?cliMetadataHeader,
        ?cliMetadataRoot,
        ?assembly,
        ?warnings,
        ?typeDefCapacity,
        ?typeRefCapacity,
        ?assemblyRefCapacity
    )
    =
    let info =
        let typeDefCapacity' = defaultArg typeDefCapacity 16
        let typeRefCapacity' = defaultArg typeRefCapacity 32

        { Header = defaultArg cliMetadataHeader CliHeader.latestDefault
          Root = defaultArg cliMetadataRoot CliMetadataRoot.defaultFields
          Name = name
          Mvid = Option.defaultWith Guid.NewGuid mvid
          NamedTypes = NamedTypeCache.empty typeDefCapacity' typeRefCapacity'
          CustomAttributes = CustomAttribute.LookupBuilder(fun ctor -> Ok ctor.Constructor.Member.ParameterTypes.Length)
          Assembly = assembly
          AssemblyReferences = HashSet<ReferencedAssembly>(defaultArg assemblyRefCapacity 8)
          DefinedTypes = Dictionary typeDefCapacity'
          NestedTypes = Dictionary()
          NonNestedTypes = List typeDefCapacity'
          ReferencedTypes = Dictionary typeRefCapacity'
          FieldReferences = ImmutableArray.CreateBuilder()
          MethodReferences = ImmutableArray.CreateBuilder()
          MethodBodies = MethodBodyStream()
          EntryPoint = ref Unchecked.defaultof<EntryPoint> }

    let createAttributeList owner = CustomAttributeList(owner, info.CustomAttributes)

    let defineMembersFor owner =
        DefinedTypeMembers(owner, warnings, info.MethodBodies, info.NamedTypes, info.EntryPoint, info.CustomAttributes)

    let mutable assemblyDefAttributes =
        match assembly with
        | Some assembly' -> Some(createAttributeList(CustomAttribute.Owner.DefinedAssembly assembly'))
        | None -> None

    do info.GlobalMembers <- defineMembersFor ModuleType.Definition'

    member val ValidationWarnings = ValidationWarningsCollection(?warnings = warnings)
    member val ModuleCustomAttributes = createAttributeList(CustomAttribute.Owner.DefinedType ModuleType.Definition')
    member _.GlobalMembers = info.GlobalMembers
    member _.Mvid = info.Mvid
    member _.Name: Identifier = name
    member _.EntryPoint = !info.EntryPoint
    member _.Assembly = info.Assembly
    member _.AssemblyCustomAttributes = assemblyDefAttributes

    member _.DefineAssembly(assembly: DefinedAssembly) =
        match info.Assembly with
        | Some existing -> ValidationResult.Error(failwith "TODO: Warning for duplicate assembly definition")
        | None ->
            let attrs = createAttributeList(CustomAttribute.Owner.DefinedAssembly assembly)
            info.Assembly <- Some assembly
            assemblyDefAttributes <- Some attrs
            Ok attrs

    member _.ReferenceAssembly assembly =
        if not (info.AssemblyReferences.Add assembly) then
            match warnings with
            | Some warnings' -> warnings'.Add(failwith "TODO: Warning for duplicate assembly reference")
            | None -> ()

    member private _.AddDefinedType tdef = // TODO: Prevent addition of type named <Module>
        match info.DefinedTypes.TryGetValue tdef with
        | true, existing -> ValidationResult<_>.Error(noImpl "TODO: error for duplicate type def")
        | false, _ ->
            // TODO: Check that extends and nestedclass are already contained in the Module.
            let members = defineMembersFor tdef
            info.DefinedTypes.Add(tdef, members)

            match tdef.EnclosingClass with
            | ValueSome parent ->
                let nested =
                    match info.NestedTypes.TryGetValue parent with
                    | true, existing -> existing
                    | false, _ ->
                        let nested' = List()
                        info.NestedTypes.Add(parent, nested')
                        nested'
                nested.Add tdef
            | ValueNone -> info.NonNestedTypes.Add tdef

            Ok members

    member this.DefineType definition =
        validated {
            let! members = this.AddDefinedType definition
            return struct(createAttributeList(CustomAttribute.Owner.DefinedType definition), members)
        }

    member this.DefineType(definition: DefinedType, attributes) =
        validated {
            let! members = this.AddDefinedType definition
            CustomAttributeList.fromRef (CustomAttribute.Owner.DefinedType definition) info.CustomAttributes attributes
            return members
        }

    member this.DefineType(definition: TypeDefinition<'Kind>, attributes) =
        validated {
            let definition' = DefinedType.Definition definition.Definition
            let! members = this.AddDefinedType definition'
            CustomAttributeList.fromRef (CustomAttribute.Owner.DefinedType definition') info.CustomAttributes attributes
            return DefinedTypeMembers<'Kind> members
        }

    // TODO: Error if a generic parameter constraint references System.Void
    member this.DefineGenericType(definition, attributes) = this.DefineType(DefinedType.Generic definition, attributes)

    member this.DefineGenericType(definition: GenericType.Definition<'Kind>, attributes) =
        validated {
            let! members = this.DefineGenericType(definition.Definition, attributes)
            return DefinedTypeMembers<'Kind> members
        }

    member _.ReferenceType reference =
        match info.ReferencedTypes.TryGetValue reference with
        | true, existing -> ValidationResult<_>.Error(noImpl "TODO: error for duplicate type ref")
        | false, _ ->
            // TODO: Check that the resolution scope is already accounted for.
            let members = ReferencedTypeMembers(reference, warnings, info.NamedTypes)
            info.ReferencedTypes.Add(reference, members)
            Ok members

    member this.ReferenceType(reference: TypeReference<'Kind>) =
        validated {
            let! members = this.ReferenceType(ReferencedType.Reference reference.Reference)
            return ReferencedTypeMembers<'Kind> members
        }

    // TODO: Implement lookup for MemberRefs created from fields and methods of generic types.

    member _.GenericInstantiation(isValueType, field: FieldTok, typeGenericParameters) =
        let inline create instantiated =
            let inst = GenericType.instantiate instantiated typeGenericParameters
            let owner = if isValueType then CliType.GenericValueType inst else CliType.GenericClass inst
            let field' = FieldTok.create (TypeTok.Specified owner) (Field(field.Member.Name, field.Member.Type)) // TODO: Make a special subclass for these kinds of fields.

            info.FieldReferences.Add { Owner = owner; Member = field'.Member }
            field'
        // TODO: Check that owner of field is accounted for, and that the type actually contains the field.
        match field.Owner with
        | TypeTok.Named(NamedType.DefinedType(DefinedType.Definition _))
        | TypeTok.Named(NamedType.ReferencedType(ReferencedType.Reference _)) -> field
        | TypeTok.Named(NamedType.DefinedType(DefinedType.Generic definition)) -> create(GenericType.Defined definition)
        | TypeTok.Named(NamedType.ReferencedType(ReferencedType.Generic reference)) -> create(GenericType.Referenced reference)
        | TypeTok.Specified tspec -> failwith "TODO: Handle usage of TypeSpec when generating MemberRef to a field of a generic type"

    member _.GenericInstantiation(isValueType, method: MethodTok, typeGenericParameters) =
        let inline create instantiated =
            let inst = GenericType.instantiate instantiated typeGenericParameters
            let owner = if isValueType then CliType.GenericValueType inst else CliType.GenericClass inst
            let method' =
                Method (
                    method.Member.HasThis,
                    method.Member.CallingConvention,
                    method.Member.Name,
                    method.Member.ReturnType,
                    method.Member.ParameterTypes
                )
                |> MethodTok.create (TypeTok.Specified owner) // TODO: Make a special subclass for these kinds of methods.

            info.MethodReferences.Add { Owner = owner; Member = method'.Member }
            method'
        match method.Owner with
        | TypeTok.Named(NamedType.DefinedType(DefinedType.Definition _))
        | TypeTok.Named(NamedType.ReferencedType(ReferencedType.Reference _)) -> method
        | TypeTok.Named(NamedType.DefinedType(DefinedType.Generic definition)) -> create(GenericType.Defined definition)
        | TypeTok.Named(NamedType.ReferencedType(ReferencedType.Generic reference)) -> create(GenericType.Referenced reference)
        | TypeTok.Specified tspec -> failwith "TODO: Handle usage of TypeSpec when generating MemberRef to a method of a generic type"

    member this.SetTargetFramework(tfm, ctor: CustomAttributeCtor) =
        match this.AssemblyCustomAttributes with
        | Some attributes' ->
            // TODO: Check that type of TargetFrameworkAttribute is correct.
            // TODO: Check that TargetFrameworkAttribute ctor defines at least one argument.
            attributes'.Add
                { Constructor = ctor
                  FixedArguments =
                    fun i _ ->
                        function
                        | _ when i > 0 -> Error(noImpl "TODO: Error for TargetFrameworkAttribute cannot have > 1 argument")
                        | ElemType.Primitive PrimitiveElemType.String -> Ok(FixedArg.Elem (Elem.SerString tfm))
                        | bad -> Error(noImpl "TODO: Error for bad TargetFrameworkAttribute argument type")
                  NamedArguments = ImmutableArray.Empty }
        | None -> Some(noImpl "TODO: Error for not assembly")

    member _.CreateMetadata() =
        let metadataTokenSource = ref Unchecked.defaultof<IMetadataTokenSource>

        let writeDefinedMethods = lazy info.MethodBodies.ToMemory !metadataTokenSource

        let builder =
            CliMetadataBuilder (
                info.Header,
                info.Root,
                lazy fst writeDefinedMethods.Value,
                (fun str guid _ -> ModuleRow.create (str.Add info.Name) (guid.Add info.Mvid)),
                StringsStreamBuilder 512,
                UserStringStreamBuilder 1,
                GuidStreamBuilder 1,
                BlobStreamBuilder 256
            )

        let assemblyDefIndex =
            match info.Assembly with
            | Some assem ->
                { HashAlgId = AssemblyHashAlgorithm.SHA1 // TODO: Set the HashAlgId properly
                  MajorVersion = assem.Version.Major
                  MinorVersion = assem.Version.Minor
                  BuildNumber = assem.Version.Build
                  RevisionNumber = assem.Version.Revision
                  Flags = if assem.PublicKey.IsDefaultOrEmpty then AssemblyFlags.None else AssemblyFlags.PublicKey // TODO: Allow setting of other assembly flags
                  PublicKey = builder.Blob.Add assem.PublicKey
                  Name = builder.Strings.Add assem.Name
                  Culture = builder.Strings.Add assem.Culture }
                |> builder.Tables.Assembly.Add
                |> ValueSome
            | None -> ValueNone

        let assemblyReferenceLookup = Dictionary<ReferencedAssembly, _> info.AssemblyReferences.Count

        let referencedTypeLookup = Dictionary<ReferencedType, _> info.ReferencedTypes.Count
        let referencedFieldLookup = Dictionary<FieldTok<ReferencedType, ReferencedField>, _>()
        let referencedMethodLookup = Dictionary<MethodTok<ReferencedType, ReferencedMethod>, _>()
        
        let serializedDefinedTypes = List<DefinedType>(info.DefinedTypes.Count + 1)
        let definedTypeLookup = Dictionary<DefinedType, _> serializedDefinedTypes.Capacity
        let definedFieldLookup = Dictionary<FieldTok<DefinedType, DefinedField>, _>()
        let definedMethodLookup = Dictionary<MethodTok<DefinedType, DefinedMethod>, _>()

        let miscFieldLookup = Dictionary<FieldTok, _>()
        let miscMethodLookup = Dictionary<MethodTok, _>()

        let genericParamLookup = ImmutableSortedDictionary.CreateBuilder<_, ImmutableArray<GenericParam>>()

        let createBlobLookup comparer writer =
            let lookup = Dictionary<'Blob, 'Offset>(comparer = comparer)
        
            fun blob ->
                match lookup.TryGetValue blob with
                | true, existing -> existing
                | false, _ ->
                    let offset = writer blob
                    lookup.Add(blob, offset)
                    offset

        let mutable serializeDefinedType = Unchecked.defaultof<DefinedType -> TableIndex<_>>
        let mutable serializeReferencedType = Unchecked.defaultof<ReferencedType -> TableIndex<_>>

        let typeDefEncoded definition =
            TypeDefOrRefEncoded.Def(serializeDefinedType definition)

        let typeRefEncoded reference =
            TypeDefOrRefEncoded.Ref(serializeReferencedType reference)

        let typeDefOrRef = // TODO: How to deal with generic types that are missing their type parameters?
            function
            | NamedType.DefinedType tdef -> typeDefEncoded tdef
            | NamedType.ReferencedType tref -> typeRefEncoded tref

        let mutable getEncodedType = Unchecked.defaultof<CliType -> EncodedType>

        let rec typeDefOrRefOrSpec =
            function
            | TypeTok.Named tdef -> TypeDefOrRefEncoded.toCodedIndex(typeDefOrRef tdef)
            | TypeTok.Specified tspec -> TypeDefOrRef.Spec(getTypeSpec(getEncodedType tspec))

        and getCustomModifiers (modifiers: ImmutableArray<ModifierType>) =
            let rec inner i modifiers' =
                if i >= 0 then
                    let modf' = modifiers.[i]
                    inner
                        (i - 1)
                        ({ CustomMod.Required = modf'.Required; ModifierType = typeDefOrRefOrSpec modf'.Modifier } :: modifiers')
                else modifiers'
            inner (modifiers.Length - 1) List.empty

        and getTypeSpec =
            createBlobLookup EqualityComparer.Default <| fun (tspec: EncodedType) ->
                builder.Tables.TypeSpec.Add { TypeSpec = builder.Blob.Add tspec }

        let getParameterTypes (parameters: ImmutableArray<ParameterType>) =
            if parameters.IsDefaultOrEmpty
            then ImmutableArray<ParamItem>.Empty
            else
                let mutable parameters' = Array.zeroCreate<ParamItem> parameters.Length

                for i = 0 to parameters'.Length - 1 do
                    parameters'.[i] <-
                        match parameters.[i] with
                        | ParameterType.T ptype ->
                            ParamItem.Type(getEncodedType ptype)
                        | ParameterType.ByRef(modifiers, ptype) ->
                            ParamItem.ByRef(getCustomModifiers modifiers, getEncodedType ptype)
                        | ParameterType.TypedByRef modifiers ->
                            ParamItem.TypedByRef(getCustomModifiers modifiers)

                Unsafe.As &parameters'

        let getMethodSig =
            createBlobLookup Method.signatureComparer <| fun (method: Method) ->
                let signature =
                    { HasThis = method.HasThis
                      CallingConvention = method.CallingConvention
                      ReturnType =
                        match method.ReturnType with
                        | ReturnType.T ret -> RetTypeItem.Type(getEncodedType ret)
                        | ReturnType.Void modifiers -> RetTypeItem.Void(getCustomModifiers modifiers)
                        | ReturnType.ByRef(modifiers, ret) -> RetTypeItem.ByRef(getCustomModifiers modifiers, getEncodedType ret)
                        | ReturnType.TypedByRef modifiers -> RetTypeItem.TypedByRef(getCustomModifiers modifiers)
                      Parameters = getParameterTypes method.ParameterTypes }
                builder.Blob.Add &signature

        let getFieldSig =
            createBlobLookup Field.signatureComparer <| fun (field: Field) ->
                let signature = { FieldType = getEncodedType field.Type }
                builder.Blob.Add &signature

        let getLocalsSig =
            let inline constraints pinned =
                if pinned then LocalVariable.pinned else List.empty

            let lookup =
                createBlobLookup Equatable.BlockComparer <| fun (locals: ImmutableArray<LocalType>) ->
                    let mutable locals' = Array.zeroCreate<LocalVariable> locals.Length

                    for i = 0 to locals'.Length - 1 do
                        locals'.[i] <-
                            match locals.[i] with
                            | LocalType.T(modifiers, pinned, ltype) ->
                                LocalVariable.Type(getCustomModifiers modifiers, constraints pinned, getEncodedType ltype)
                            | LocalType.ByRef(modifiers, pinned, ltype) ->
                                LocalVariable.ByRef(getCustomModifiers modifiers, constraints pinned, getEncodedType ltype)
                            | LocalType.TypedByRef modifiers ->
                                LocalVariable.TypedByRef(getCustomModifiers modifiers)

                    builder.Tables.StandAloneSig.Add { StandAloneSig = builder.Blob.Add(Unsafe.As<_, LocalVarSig> &locals') }

            fun (locals: ImmutableArray<_>) ->
                if locals.IsDefaultOrEmpty
                then { TableIndex = 0u }
                else lookup locals

        serializeDefinedType <- fun definition ->
            match definedTypeLookup.TryGetValue definition with
            | true, existing -> existing
            | false, _ ->
                let definition' =
                    match definition with
                    | DefinedType.Definition tdef
                    | DefinedType.Generic(GenericType(tdef, _)) -> tdef

                let extends =
                    match definition'.Extends.Extends with
                    | ValueSome(TypeTok.Named tnamed) -> TypeDefOrRefEncoded.toCodedIndex(typeDefOrRef tnamed)
                    | ValueSome(TypeTok.Specified tspec) -> TypeDefOrRef.Spec(getTypeSpec(getEncodedType tspec))
                    | ValueNone -> Unchecked.defaultof<TypeDefOrRef> // Null

                // The defined fields and methods are added later on in serialization to avoid infinite loops. If the members
                // were added here instead, any MemberRef used in one of the methods would need to be resolved, but the type has
                // not been added yet, so it attempts to add all of the members again, then sees that the same MemberRef has
                // still not been resolved, looping forever.
                let i =
                    builder.Tables.TypeDef.Add
                        { Flags = definition'.Flags
                          TypeName = builder.Strings.Add definition'.TypeName
                          TypeNamespace = builder.Strings.Add definition'.TypeNamespace
                          Extends = extends
                          FieldList = Unchecked.defaultof<TableIndex<_>>
                          MethodList = Unchecked.defaultof<TableIndex<_>> }

                definedTypeLookup.[definition] <- i
                serializedDefinedTypes.Add definition

                match definition with
                | DefinedType.Generic generic -> genericParamLookup.Add(TypeOrMethodDef.Type i, generic.Parameters)
                | _ -> ()

                i

        serializeReferencedType <- fun reference ->
            match referencedTypeLookup.TryGetValue reference with
            | true, existing -> existing
            | false, _ ->
                let members = info.ReferencedTypes.[reference]

                let reference' =
                    match reference with
                    | ReferencedType.Reference tref
                    | ReferencedType.Generic(GenericType(tref, _)) -> tref

                let rscope =
                    match reference'.ResolutionScope with
                    | TypeReferenceParent.Assembly assem -> ResolutionScope.AssemblyRef assemblyReferenceLookup.[assem]
                    | TypeReferenceParent.Type parent ->
                        ResolutionScope.TypeRef(serializeReferencedType parent)

                let i =
                    builder.Tables.TypeRef.Add
                        { ResolutionScope = rscope
                          TypeName = builder.Strings.Add reference'.TypeName
                          TypeNamespace = builder.Strings.Add reference'.TypeNamespace }

                for field in members.Field do
                    referencedFieldLookup.[FieldTok.ofTypeRef reference field info.NamedTypes] <-
                        builder.Tables.MemberRef.Add
                            { Class = MemberRefParent.TypeRef i
                              Name = builder.Strings.Add field.Name
                              Signature = { MemberRefSig = getFieldSig(field).FieldSig } }

                for method in members.Method do
                    referencedMethodLookup.[MethodTok.ofTypeRef reference method info.NamedTypes] <-
                        builder.Tables.MemberRef.Add
                            { Class = MemberRefParent.TypeRef i
                              Name = builder.Strings.Add method.Name
                              Signature =
                                // TODO: Use separate lookup if method reference signature has any VarArgs.
                                { MemberRefSig = getMethodSig(method).MethodDefSig } }

                referencedTypeLookup.[reference] <- i
                i

        getEncodedType <- // TODO: Implement replacement of long form for primitive types.
            let getGenericInst isValueType (inst: GenericTypeInstantiation) =
                let arguments = &inst.Arguments
                let mutable arguments' = Array.zeroCreate<EncodedType> arguments.Count

                for i = 0 to arguments'.Length - 1 do arguments'.[i] <- getEncodedType arguments.[i]

                { IsValueType = isValueType
                  GenericInst.GenericType =
                    match inst.Instantiated with
                    | GenericType.Defined definition -> typeDefEncoded(DefinedType.Generic definition)
                    | GenericType.Referenced reference -> typeRefEncoded(ReferencedType.Generic reference)
                  GenericArguments = { GenericArgList.GenArgs = Unsafe.As &arguments' } }

            let lookup =
                createBlobLookup EqualityComparer.Default <| // TODO: Make a EncodedTypeCache class, and use it here.
                    function
                    | CliType.Class tdef -> EncodedType.Class(typeDefOrRef tdef)
                    | CliType.ValueType tdef -> EncodedType.ValueType(typeDefOrRef tdef)
                    | CliType.GenericClass inst -> EncodedType.GenericInst(getGenericInst false inst)
                    | CliType.GenericValueType inst -> EncodedType.GenericInst(getGenericInst true inst)
                    | CliType.SZArray elem -> EncodedType.SZArray(getEncodedType elem)
                    | CliType.Modified(modifiers, elem) when modifiers.IsDefaultOrEmpty -> getEncodedType elem
                    | CliType.Modified(modifiers, elem) -> EncodedType.Modified(getCustomModifiers modifiers, getEncodedType elem)
                    | CliType.Array(elem, shape) -> EncodedType.Array(getEncodedType elem, shape)
                    | CliType.TypeVar(GenericParamIndex i) -> EncodedType.Var(uint32 i)
                    | CliType.Primitive prim -> invalidOp(sprintf "Primitive types \"%A\" should not be cached" prim)

            function // Ensures that only non-primitive types are cached.
            | CliType.Primitive prim -> prim.Encoded
            | cached -> lookup cached

        for assem in info.AssemblyReferences do
            assemblyReferenceLookup.[assem] <-
                builder.Tables.AssemblyRef.Add
                    { MajorVersion = assem.Version.Major
                      MinorVersion = assem.Version.Minor
                      BuildNumber = assem.Version.Build
                      RevisionNumber = assem.Version.Revision
                      PublicKeyOrToken = builder.Blob.Add assem.PublicKeyOrToken
                      Name = builder.Strings.Add assem.Name
                      Culture = builder.Strings.Add assem.Culture
                      HashValue = builder.Blob.Add assem.HashValue }

        for tref in info.ReferencedTypes.Keys do serializeReferencedType tref |> ignore

        serializeDefinedType ModuleType.Definition' |> ignore // Serialize the <Module> special class.

        match info.NestedTypes.TryGetValue ModuleType.Definition' with
        | true, existing -> for child in existing do serializeDefinedType child |> ignore
        | false, _ -> ()

        // Enclosing classes "precede the definition of all classes it encloses" (II.22)
        for KeyValue(parent, nested) in info.NestedTypes do
            if Object.ReferenceEquals(parent, ModuleType.Definition') |> not then
                serializeDefinedType parent |> ignore
                for child in nested do serializeDefinedType child |> ignore

        for tdef in info.NonNestedTypes do serializeDefinedType tdef |> ignore

        metadataTokenSource :=
            { new IMetadataTokenSource with
                member _.GetLocalVariables locals = getLocalsSig locals

                member _.GetUserString(str: inref<ReadOnlyMemory<char>>) = builder.UserString.AddFolded str

                member _.GetFieldToken field =
                    match field.Owner with
                    | TypeTok.Named(NamedType.DefinedType _) ->
                        FieldMetadataToken.Def definedFieldLookup.[FieldTok.unsafeAs field]
                    | TypeTok.Named(NamedType.ReferencedType _) ->
                        FieldMetadataToken.Ref referencedFieldLookup.[FieldTok.unsafeAs field]
                    | TypeTok.Specified _ -> FieldMetadataToken.Ref miscFieldLookup.[field]

                member _.GetMethodToken method =
                    match method.Owner with
                    | TypeTok.Named(NamedType.DefinedType _) ->
                        MethodMetadataToken.Def definedMethodLookup.[MethodTok.unsafeAs method]
                    | TypeTok.Named(NamedType.ReferencedType _) ->
                        MethodMetadataToken.Ref referencedMethodLookup.[MethodTok.unsafeAs method]
                    | TypeTok.Specified _ -> // TODO: Use a Ref or Spec depending on if the method itself has generic parameters.
                        MethodMetadataToken.Ref miscMethodLookup.[method]

                member _.GetTypeToken t = TypeMetadataToken.ofCodedIndex(typeDefOrRefOrSpec t) }

        let parameterList = MemberIndexCounter<ParamRow>()
        let fieldList = MemberIndexCounter<FieldRow>()
        let methodList = MemberIndexCounter<MethodDefRow>()
        //property and event

        let serializeDefinedMembers parent members =
            let members' =
                match members with
                | ValueNone -> info.DefinedTypes.[parent]
                | ValueSome members -> members

            fieldList.Reset()

            for field in members'.Field do
                let i' =
                    builder.Tables.Field.Add
                        { Flags = field.Flags
                          Name = builder.Strings.Add field.Name
                          Signature = getFieldSig field }

                fieldList.Last <- i'
                definedFieldLookup.[FieldTok.ofTypeDef parent field info.NamedTypes] <- i'

            methodList.Reset()

            for method in members'.Method do
                parameterList.Reset()

                for i = 0 to method.Parameters.Length - 1 do
                    let param = &method.Parameters.ItemRef i
                    parameterList.Last <-
                        builder.Tables.Param.Add
                            { Flags = Parameter.flags &param
                              Name = builder.Strings.Add param.ParamName
                              Sequence = Checked.uint16(i + 1) }

                let i' =
                    { Rva =
                        match members'.MethodBodyLookup.TryGetValue method with
                        | true, body -> (snd writeDefinedMethods.Value).[body]
                        | false, _ -> Unchecked.defaultof<_>
                      ImplFlags = method.ImplFlags
                      Flags = method.Flags
                      Name = builder.Strings.Add method.Name
                      Signature = getMethodSig method
                      ParamList = parameterList.First }
                    |> builder.Tables.MethodDef.Add

                methodList.Last <- i'
                definedMethodLookup.[MethodTok.ofTypeDef parent method info.NamedTypes] <- i'

                // TODO: Add the methods generic parameters, if any.

            // TODO: event and property

            // Safe to manipulate TypeDef row here, as all type definitions have already been added.
            let parent' = &Unsafe.AsRef &builder.Tables.TypeDef.[definedTypeLookup.[parent]]

            Unsafe.AsRef &parent'.FieldList <- fieldList.First
            Unsafe.AsRef &parent'.MethodList <- methodList.First

        serializeDefinedMembers ModuleType.Definition' (ValueSome info.GlobalMembers)

        for i = 1 to serializedDefinedTypes.Count - 1 do serializeDefinedMembers serializedDefinedTypes.[i] ValueNone

        let miscMemberParent owner = getEncodedType owner |> getTypeSpec |> MemberRefParent.TypeSpec

        for i = 0 to info.FieldReferences.Count - 1 do
            let field = &info.FieldReferences.ItemRef i
            let token = FieldTok.create (TypeTok.Specified field.Owner) field.Member
            let i' =
                { Class = miscMemberParent field.Owner
                  Name = builder.Strings.Add field.Member.Name
                  Signature = { MemberRefSig = (getFieldSig field.Member).FieldSig } }
                |> builder.Tables.MemberRef.Add

            miscFieldLookup.Add(token, i')

        for i = 0 to info.MethodReferences.Count - 1 do
            let method = &info.MethodReferences.ItemRef i
            let token = MethodTok.create (TypeTok.Specified method.Owner) method.Member
            let i' =
                { Class = miscMemberParent method.Owner
                  Name = builder.Strings.Add method.Member.Name
                  Signature = { MemberRefSig = (getMethodSig method.Member).MethodDefSig } }
                |> builder.Tables.MemberRef.Add

            miscMethodLookup.Add(token, i')

        for KeyValue(owner, parameters) in genericParamLookup do
            for i = 0 to parameters.Length - 1 do
                let parameter = &parameters.ItemRef i

                let parami =
                    { Number = uint16 i
                      Flags = parameter.Flags
                      Owner = owner
                      Name = builder.Strings.Add parameter.Name }
                    |> builder.Tables.GenericParam.Add

                for constr in parameter.Constraints do
                    { Owner = parami
                      Constraint = typeDefOrRefOrSpec constr }
                    |> builder.Tables.GenericParamConstraint.Add
                    |> ignore

        if builder.Tables.GenericParam.Count > 0 then
            builder.Tables.Sorted <- builder.Tables.Sorted ||| ValidTableFlags.GenericParam

        if builder.Tables.GenericParamConstraint.Count > 0 then
            builder.Tables.Sorted <- builder.Tables.Sorted ||| ValidTableFlags.GenericParamConstraint

        if info.CustomAttributes.OwnerCount > 0 then
            let lookup = SortedList<HasCustomAttribute, _> info.CustomAttributes.OwnerCount

            for KeyValue({ CustomAttribute.Owner = parent }, attrs) in info.CustomAttributes do
                let index =
                    if Object.ReferenceEquals(parent, ModuleType.Definition') then
                        HasCustomAttribute.Module
                    elif info.Assembly.IsSome && Object.ReferenceEquals(parent, info.Assembly.Value) then
                        HasCustomAttribute.Assembly assemblyDefIndex.Value
                    else
                        match parent with
                        | _ ->
                            parent.GetType().Name
                            |> sprintf "Unsupported custom attribute parent type %s"
                            |> invalidOp
                lookup.[index] <- attrs

            for KeyValue(parent, attrs: ImmutableArray<CustomAttribute.Constructed>.Builder) in lookup do
                for i = 0 to attrs.Count - 1 do
                    let attribute = &attrs.ItemRef i
                    { Parent = parent
                      Type =
                        let ctor = attribute.Constructor.Constructor
                        match ctor.Owner with
                        | TypeTok.Named(NamedType.DefinedType _) ->
                            CustomAttributeType.MethodDef definedMethodLookup.[MethodTok.unsafeAs ctor]
                        | TypeTok.Named(NamedType.ReferencedType _) ->
                            CustomAttributeType.MemberRef referencedMethodLookup.[MethodTok.unsafeAs ctor]
                        | TypeTok.Specified _ -> // TODO: If CA constructor defines generic parameters, use MethodSpec instead.
                            CustomAttributeType.MemberRef miscMethodLookup.[ctor]
                      Value = builder.Blob.Add &attribute.Signature }
                    |> builder.Tables.CustomAttribute.Add
                    |> ignore

            builder.Tables.Sorted <- builder.Tables.Sorted ||| ValidTableFlags.CustomAttribute

        builder.EntryPointToken <-
            match !info.EntryPoint with
            | NoEntryPoint -> EntryPointToken.Null
            | EntryPointMethod call -> EntryPointToken.MethodDef definedMethodLookup.[MethodTok.unsafeAs call.Token]

        builder
