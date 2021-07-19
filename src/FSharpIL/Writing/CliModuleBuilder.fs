﻿namespace FSharpIL.Writing

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Cil
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

open FSharpIL.Writing.Cil

open FSharpIL.Utilities
open FSharpIL.Utilities.Collections
open FSharpIL.Utilities.Compare

[<AbstractClass>]
type DefinedMethodBody =
    val LocalTypes: ImmutableArray<LocalVariableType>
    val InitLocals: InitLocals

    new (localTypes, initLocals) = { LocalTypes = localTypes; InitLocals = initLocals }
    new (localTypes) = DefinedMethodBody(localTypes, SkipInitLocals)
    new () = DefinedMethodBody ImmutableArray.Empty

    /// Writes the instructions of the method body, and returns the maximum number of items on the stack.
    abstract WriteInstructions: byref<MethodBodyBuilder> * MetadataTokenSource -> uint16

type EntryPoint =
    private
    | NoEntryPoint
    | EntryPointMethod of MethodCallTarget<TypeDefinition, MethodDefinition<MethodKinds.Static>>

[<RequireQualifiedAccess>]
module EntryPoint =
    let (|None|Method|) entryPoint =
        match entryPoint with
        | NoEntryPoint -> None
        | EntryPointMethod(MethodCallTarget(owner, method)) -> Method(struct(owner, FSharpIL.Cli.EntryPointMethod method))

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

        static member DefinedAssembly (assembly: DefinedAssembly) = { Owner = assembly }
        static member DefinedType (tdef: TypeDefinition) = { Owner = tdef }
        static member DefinedMethod (mdef: DefinedMethod) = { Owner = mdef }

    let rec private createFixedArgsLoop
        (args: byref<FixedArg[]>)
        (parameterTypes: ImmutableArray<MethodParameterType>)
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
            | MethodParameterType.Type t ->
                match NamedType.toElemType t with
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

    let private createFixedArgs source numFixedArgs customAttributeCtor =
        let mutable fixedArgs = Array.zeroCreate numFixedArgs

        let parameterTypes, parameterNames =
            match customAttributeCtor with
            | CustomAttributeCtor.Ref(MethodCallTarget.Callee ctor) -> ctor.ParameterTypes, ImmutableArray.Empty
            | CustomAttributeCtor.Def(MethodCallTarget.Callee ctor) -> ctor.ParameterTypes, ctor.Parameters

        createFixedArgsLoop &fixedArgs parameterTypes parameterNames source 0

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

[<Sealed>]
type DefinedTypeMembers (owner: TypeDefinition, warnings: ValidationWarningsBuilder option, entryPointToken: EntryPoint ref, attrs) =
    [<DefaultValue>] val mutable internal Field: HybridHashSet<DefinedField>
    [<DefaultValue>] val mutable Method: HybridHashSet<DefinedMethod>
    [<DefaultValue>] val mutable MethodBodyLookup: LateInitDictionary<DefinedMethod, DefinedMethodBody>

    member this.FieldCount = this.Field.Count
    member this.MethodCount = this.Method.Count

    member this.AddDefinedMethod(method, body: DefinedMethodBody voption) =
        // TODO: Check that owner is the correct kind of type to own this kind of method.
        if this.Method.Add method then
            match body with
            | ValueSome body' -> this.MethodBodyLookup.[method] <- body'
            | ValueNone -> ()

            Ok(MethodCallTarget<TypeDefinition, DefinedMethod>(owner, method))
        else ValidationResult.Error(noImpl "error for duplicate method")

    member this.DefineMethod(method, body, attributes) =
        validated {
            let! target = this.AddDefinedMethod(method, body)
            CustomAttributeList.fromRef (CustomAttribute.Owner.DefinedMethod method) attrs attributes
            return target
        }

    member this.DefineEntryPoint(method: EntryPointMethod, body, attributes) =
        validated {
            let! target = this.DefineMethod(method.Method, ValueSome body, attributes)
            let target' = MethodCallTarget<_, _>(target.Owner, method.Method)
            entryPointToken := EntryPoint.EntryPointMethod target'
            return target'
        }

    member this.ContainsField field = this.Field.Contains field
    member this.ContainsMethod method = this.Method.Contains method

[<Sealed>]
type ReferencedTypeMembers =
    val private owner: TypeReference
    val private warnings: ValidationWarningsBuilder option
    [<DefaultValue>] val mutable Field: HybridHashSet<ReferencedField>
    [<DefaultValue>] val mutable Method: HybridHashSet<ReferencedMethod>

    new (owner, warnings) = { owner = owner; warnings = warnings }

    member this.FieldCount = this.Field.Count
    member this.MethodCount = this.Method.Count

    member this.ReferenceMethod(method: ReferencedMethod) =
        // TODO: Check that the kind and owner of method are correct.
        if this.Method.Add method
        then ValidationResult.Ok(MethodCallTarget<TypeReference, ReferencedMethod>(this.owner, method))
        else Error(noImpl "error for duplicate method")

    member this.ContainsField field = this.Field.Contains field
    member this.ContainsMethod method = this.Method.Contains method

[<Struct>]
type MemberIndices =
    { mutable FieldList: TableIndex<FieldRow>
      mutable MethodList: TableIndex<MethodDefRow>
      mutable EventList: TableIndex<EventRow>
      mutable PropertyList: TableIndex<PropertyRow> }

[<IsReadOnly; Struct>]
type DefinedMethodBodyWriter
    (
        localVarSource: ImmutableArray<LocalVariableType> -> TableIndex<StandaloneSigRow>,
        metadataTokenSource: MetadataTokenSource,
        body: DefinedMethodBody
    )
    =
    interface IMethodBodySource with
        member _.Create builder =
            { InitLocals = body.InitLocals
              MaxStack = body.WriteInstructions(&builder, metadataTokenSource)
              LocalVariables = localVarSource body.LocalTypes }

[<AutoOpen>]
module Helpers =
    type Dictionary<'Key, 'Value> with
        member inline this.EnsureAdditionalCapacity additional = this.EnsureCapacity(this.Count + additional)

type ModuleBuilderSerializer
    (
        cliMetadataHeader,
        cliMetadataRoot,
        name: Identifier,
        mvid,
        attributes: CustomAttribute.LookupBuilder,
        gparams: Dictionary<_, _>,
        assembly: DefinedAssembly option,
        entryPointToken: EntryPoint,
        assemblyReferences: HashSet<ReferencedAssembly>,
        definedTypes: Dictionary<TypeDefinition, DefinedTypeMembers>,
        moduleGlobalMembers: DefinedTypeMembers,
        referencedTypes: Dictionary<TypeReference, ReferencedTypeMembers>
    )
    as serializer
    = // TODO: Move this all into the Serialize method.
    let builder =
        CliMetadataBuilder (
            cliMetadataHeader,
            cliMetadataRoot,
            FSharpIL.Writing.Cil.MethodBodyList(),
            (fun str guid _ -> ModuleRow.create (str.Add name) (guid.Add mvid)),
            StringsStreamBuilder 512,
            UserStringStreamBuilder 1,
            GuidStreamBuilder 1,
            BlobStreamBuilder 256
        )

    let assemblyDefIndex =
        match assembly with
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

    let assemblyReferenceLookup = Dictionary<ReferencedAssembly, TableIndex<AssemblyRefRow>> assemblyReferences.Count

    let referencedTypeLookup = Dictionary<TypeReference, _> referencedTypes.Count
    let referencedFieldLookup = Dictionary<FieldArg<TypeReference, ReferencedField>, _>()
    let referencedMethodLookup = Dictionary<MethodCallTarget<TypeReference, ReferencedMethod>, _>()

    let definedTypeLookup = Dictionary<TypeDefinition, _> definedTypes.Count
    let definedFieldLookup = Dictionary<FieldArg<TypeDefinition, DefinedField>, _>()
    let definedMethodLookup = Dictionary<MethodCallTarget<TypeDefinition, DefinedMethod>, _>()

    let createBlobLookup comparer writer =
        let lookup = Dictionary<'Blob, 'Offset>(comparer = comparer)

        fun blob ->
            match lookup.TryGetValue blob with
            | true, existing -> existing
            | false, _ ->
                let offset = writer blob
                lookup.[blob] <- offset
                offset

    let typeDefEncoded definition =
        TypeDefOrRefEncoded.Def(serializer.SerializeDefinedType(definition, definedTypes.[definition]))

    let typeRefEncoded reference =
        TypeDefOrRefEncoded.Ref(serializer.SerializeReferencedType reference)

    let typeDefOrRef: NamedType -> _ =
        function
        | :? TypeDefinition as tdef -> typeDefEncoded tdef
        | :? ReferencedType as tref -> typeRefEncoded tref
        | bad -> invalidOp(sprintf "Expected defined or referenced type but got %A" bad)

    let rec typeDefOrRefOrSpec: NamedType -> _ =
        function
        | :? TypeDefinition as tdef -> TypeDefOrRef.Def(serializer.SerializeDefinedType(tdef, definedTypes.[tdef]))
        | :? ReferencedType as tref -> TypeDefOrRef.Ref(serializer.SerializeReferencedType tref)
        | tspec ->  TypeDefOrRef.Spec(getTypeSpec(getEncodedType tspec))

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

    // TODO: How to decide if VALUETYPE or CLASS should be used? Maybe make a ClassType and ValueType derived from NamedType while also checking if a DefinedType is derived from ValueType or Enum
    and getEncodedType (t: NamedType) = // TODO: Implement caching for EncodedType instances.
        match t with
        | :? PrimitiveType as prim -> prim.Encoded
        | :? DefinedType as tdef -> EncodedType.Class(typeDefEncoded tdef)
        | :? ReferencedType as tref -> EncodedType.Class(typeRefEncoded tref)
        | :? SZArrayType as arr -> EncodedType.SZArray(getEncodedType arr.ElementType)
        | :? ArrayType as arr -> EncodedType.Array(getEncodedType arr.ElementType, arr.Shape)
        | :? InstantiatedType<DefinedType> as inst ->
            if inst.Arguments.Count > 0 then
                let mutable arguments = Array.zeroCreate<EncodedType> inst.Arguments.Count
                for i = 0 to arguments.Length - 1 do arguments.[i] <- getEncodedType inst.Arguments.[i]
                GenericInst.Class(typeDefEncoded inst.Instantiated, failwith "bad") |> EncodedType.GenericInst
            else getEncodedType inst.Instantiated
        // :? InstantiatedType<ReferencedType>
        | :? ModifiedType as modf when modf.Modifiers.Length > 0 ->
            EncodedType.Modified(getCustomModifiers modf.Modifiers, getEncodedType modf.Modified)
        | :? ModifiedType as modf -> getEncodedType modf.Modified
        | _ -> invalidOp(sprintf "Cannot encode unsupported type %s" (t.GetType().Name))

    let getParameterTypes (parameters: ImmutableArray<MethodParameterType>) =
        if parameters.IsDefaultOrEmpty
        then ImmutableArray<ParamItem>.Empty
        else
            let mutable parameters' = Array.zeroCreate<ParamItem> parameters.Length

            for i = 0 to parameters'.Length - 1 do
                let param = &parameters.ItemRef i
                let paramType =
                    match param.Type with
                    | ValueSome paramType' -> ValueSome(getEncodedType paramType')
                    | ValueNone -> ValueNone

                parameters'.[i] <- ParamItem(param.Tag, getCustomModifiers param.CustomModifiers, paramType)

            Unsafe.As &parameters'

    let getMethodSig =
        createBlobLookup Method.signatureComparer <| fun (method: Method) ->
            let signature =
                { HasThis = method.HasThis
                  CallingConvention = method.CallingConvention
                  ReturnType =
                    let returnType = &method.ReturnType
                    let returnType' =
                        match returnType.Type with
                        | ValueSome returnType' -> ValueSome(getEncodedType returnType')
                        | ValueNone -> ValueNone

                    ReturnType(returnType.Tag, getCustomModifiers returnType.CustomModifiers, returnType')
                  Parameters = getParameterTypes method.ParameterTypes }
            builder.Blob.Add &signature

    let getFieldSig =
        createBlobLookup Field.signatureComparer <| fun (field: Field) ->
            let signature = { FieldType = getEncodedType field.Type }
            builder.Blob.Add &signature

    let getLocalsSig =
        let lookup =
            createBlobLookup Equatable.BlockComparer <| fun (locals: ImmutableArray<LocalVariableType>) ->
                let mutable locals' = Array.zeroCreate<LocalVariable> locals.Length

                for i = 0 to locals'.Length - 1 do
                    let lvar = &locals.ItemRef i
                    let constraints = if lvar.IsPinned then LocalVariable.pinned else List.empty
                    let ltype =
                        match lvar.Type with
                        | ValueSome ltype' -> ValueSome(getEncodedType ltype')
                        | ValueNone -> ValueNone

                    locals'.[i] <- LocalVariable(getCustomModifiers lvar.CustomModifiers, constraints, lvar.Tag, ltype)

                failwith "TODO: Add locals to blob stream": TableIndex<StandaloneSigRow>
                //builder.Blob.Add(Unsafe.As &locals')

        fun (locals: ImmutableArray<_>) ->
            if locals.IsDefaultOrEmpty
            then { TableIndex = 0u }
            else lookup locals

    let mutable methodDefParams = TableIndex<ParamRow>.One

    // TODO: Fix, a typedef with no members may mess up the member list (previous type has one member missing).
    let mutable indices =
        { FieldList = TableIndex.One
          MethodList = TableIndex.One
          EventList = TableIndex.One
          PropertyList = TableIndex.One }

    let metadataTokenSource =
        { new MetadataTokenSource() with
            member _.GetUserString(str: string) = builder.UserString.AddFolded str
            member _.GetUserString(str: inref<ReadOnlyMemory<char>>) = builder.UserString.AddFolded str

            member _.GetFieldToken field =
                match field.Owner :> NamedType with
                | :? TypeDefinition as tdef ->
                    FieldMetadataToken.Def definedFieldLookup.[FieldArg<_, _>(tdef, Unsafe.As field.Field)]
                | :? ReferencedType as tref ->
                    FieldMetadataToken.Ref referencedFieldLookup.[FieldArg<_, _>(tref, Unsafe.As field.Field)]

            member _.GetMethodToken method =
                match method.Owner :> NamedType with
                | :? TypeDefinition as tdef ->
                    MethodMetadataToken.Def definedMethodLookup.[MethodCallTarget<_, _>(tdef, Unsafe.As method.Method)]
                | :? ReferencedType as tref ->
                    MethodMetadataToken.Ref referencedMethodLookup.[MethodCallTarget<_, _>(tref, Unsafe.As method.Method)]

            member _.GetTypeToken t = TypeMetadataToken.ofCodedIndex(typeDefOrRefOrSpec t) }

    member private _.SerializeAssemblyReferences() =
        for assem in assemblyReferences do
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

    member private this.SerializeReferencedType(reference: TypeReference) =
        let members = referencedTypes.[reference]
        match referencedTypeLookup.TryGetValue reference with
        | true, i -> i
        | false, _ ->
            let rscope =
                match reference.ResolutionScope with
                | TypeReferenceParent.Null -> ResolutionScope.Null
                | TypeReferenceParent.Assembly assem -> ResolutionScope.AssemblyRef assemblyReferenceLookup.[assem]
                | TypeReferenceParent.Type parent ->
                    ResolutionScope.TypeRef(this.SerializeReferencedType parent)

            let i =
                builder.Tables.TypeRef.Add
                    { ResolutionScope = rscope
                      TypeName = builder.Strings.Add reference.TypeName
                      TypeNamespace = builder.Strings.Add reference.TypeNamespace }

            for field in members.Field do
                referencedFieldLookup.[FieldArg<_, _>(reference, field)] <-
                    builder.Tables.MemberRef.Add
                        { Class = MemberRefParent.TypeRef i
                          Name = builder.Strings.Add field.Name
                          Signature = { MemberRefSig = getFieldSig(field).FieldSig } }

            for method in members.Method do
                referencedMethodLookup.[MethodCallTarget<_, _>(reference, method)] <-
                    builder.Tables.MemberRef.Add
                        { Class = MemberRefParent.TypeRef i
                          Name = builder.Strings.Add method.Name
                          Signature =
                            // TODO: Use separate lookup if method reference signature has any VarArgs.
                            { MemberRefSig = getMethodSig(method).MethodDefSig } }

            referencedTypeLookup.[reference] <- i
            i

    member private this.SerializeDefinedType(definition, members: DefinedTypeMembers) =
        match definedTypeLookup.TryGetValue definition with
        | true, i -> i
        | false, _ ->
            let extends =
                match definition.Extends with
                | ClassExtends.Null -> Unchecked.defaultof<TypeDefOrRef>
                | ClassExtends.Defined tdef -> TypeDefOrRef.Def(this.SerializeDefinedType(tdef, definedTypes.[tdef]))
                | ClassExtends.Referenced tref  -> TypeDefOrRef.Ref(this.SerializeReferencedType tref)
                // TODO: Handle generic instantiations

            let mutable fieldWasAdded = false

            for field in members.Field do
                let i' =
                    builder.Tables.Field.Add
                        { Flags = field.Flags
                          Name = builder.Strings.Add field.Name
                          Signature = getFieldSig field }

                if not fieldWasAdded then
                    fieldWasAdded <- true
                    indices.FieldList <- i'

                definedFieldLookup.[FieldArg<_, _>(definition, field)] <- i'

            let mutable methodWasAdded = false

            for method in members.Method do
                // TODO: If method list is empty, maybe increment methodDefParams by one, since method list of previous type will be reduced.
                for i = 0 to method.Parameters.Length - 1 do
                    let param = &method.Parameters.ItemRef i
                    let i' =
                        builder.Tables.Param.Add
                            { Flags = Parameter.flags &param
                              Name = builder.Strings.Add param.ParamName
                              Sequence = Checked.uint16(i + 1) }
                    if i = 0 then methodDefParams <- i'

                let i =
                    { Rva =
                        match members.MethodBodyLookup.TryGetValue method with
                        | true, body' ->
                            let writer =
                                DefinedMethodBodyWriter ( // TODO: How to prevent module from being modified while method bodies are being written?
                                    getLocalsSig,
                                    metadataTokenSource,
                                    body'
                                )

                            builder.MethodBodies.Add &writer
                        | false, _ -> MethodBodyLocation 0u
                      ImplFlags = method.ImplFlags
                      Flags = method.Flags
                      Name = builder.Strings.Add method.Name
                      Signature = getMethodSig method
                      ParamList = methodDefParams }
                    |> builder.Tables.MethodDef.Add

                if not methodWasAdded then
                    methodWasAdded <- true
                    indices.MethodList <- i

                definedMethodLookup.[MethodCallTarget<_, _>(definition, method)] <- i

            let i =
                builder.Tables.TypeDef.Add
                    { Flags = definition.Flags
                      TypeName = builder.Strings.Add definition.TypeName
                      TypeNamespace = builder.Strings.Add definition.TypeNamespace
                      Extends = extends
                      FieldList = indices.FieldList
                      MethodList = indices.MethodList }

            definedTypeLookup.[definition] <- i
            i

    member private _.SerializeCustomAttributes() =
        if attributes.OwnerCount > 0 then
            let lookup = SortedList<HasCustomAttribute, _> attributes.OwnerCount

            for KeyValue({ CustomAttribute.Owner = parent }, attrs) in attributes do
                let index =
                    if Object.ReferenceEquals(parent, ModuleType) then
                        HasCustomAttribute.Module
                    elif assembly.IsSome && Object.ReferenceEquals(parent, assembly.Value) then
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
                        match attribute.Constructor with
                        | CustomAttributeCtor.Ref call ->
                            CustomAttributeType.MemberRef referencedMethodLookup.[MethodCallTarget.convert call]
                        | CustomAttributeCtor.Def call ->
                            CustomAttributeType.MethodDef definedMethodLookup.[MethodCallTarget.convert call]
                      Value = builder.Blob.Add &attribute.Signature }
                    |> builder.Tables.CustomAttribute.Add
                    |> ignore

            builder.Tables.Sorted <- builder.Tables.Sorted ||| ValidTableFlags.CustomAttribute

    member this.Serialize() =
        this.SerializeAssemblyReferences()

        for tref in referencedTypes.Keys do this.SerializeReferencedType tref |> ignore

        this.SerializeDefinedType(ModuleType, moduleGlobalMembers) |> ignore

        for KeyValue(tdef, members) in definedTypes do this.SerializeDefinedType(tdef, members) |> ignore

        builder.EntryPointToken <-
            match entryPointToken with
            | NoEntryPoint -> EntryPointToken.Null
            | EntryPointMethod call -> EntryPointToken.MethodDef definedMethodLookup.[MethodCallTarget.convert call]

        //this.SerializeGenericParameters()
        this.SerializeCustomAttributes()

        builder

[<IsReadOnly; Struct>]
type ReferencedTypeMembers<'Kind when 'Kind :> IAttributeTag<TypeDefFlags>> =
    val Members: ReferencedTypeMembers

    new (members) = { Members = members }

    member this.ReferenceMethod<'Method when 'Method :> ReferencedMethod and 'Method : not struct>(method: 'Method) =
        match this.Members.ReferenceMethod method with
        | Ok target -> Ok(MethodCallTarget<TypeReference<'Kind>, 'Method>(Unsafe.As target.Owner, method))
        | Error err -> Error err

[<AbstractClass; Sealed>]
type TypeMemberExtensions = // TODO: For these extension methods, call internal version that skips some validation.
    static member ReferenceMethod(members: ReferencedTypeMembers<'Kind> when 'Kind :> TypeKinds.IHasConstructor, method) =
        members.ReferenceMethod<MethodReference<MethodKinds.ObjectConstructor>> method

    static member ReferenceMethod(members: ReferencedTypeMembers<'Kind> when 'Kind :> TypeAttributes.IHasStaticMethods, method) =
        members.ReferenceMethod<MethodReference<MethodKinds.Static>> method

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
    let cliMetadataHeader' = defaultArg cliMetadataHeader CliHeader.latestDefault
    let cliMetadataRoot' = defaultArg cliMetadataRoot CliMetadataRoot.defaultFields
    let mutable assemblyDef = assembly
    let assemblyRefs = HashSet<ReferencedAssembly>(defaultArg assemblyRefCapacity 8)
    let definedTypes = Dictionary<TypeDefinition, DefinedTypeMembers>(defaultArg typeDefCapacity 16)
    let referencedTypes = Dictionary<TypeReference, ReferencedTypeMembers>(defaultArg typeRefCapacity 32)
    let genericParameterLists = Dictionary<obj, GenericParamList> 16 // TODO: Make a GenericParamOwner struct.
    let entryPointToken = ref Unchecked.defaultof<EntryPoint>

    let customAttributeResolver =
        function
        | CustomAttributeCtor.Def(MethodCallTarget(tdef, ctor)) ->
            match definedTypes.TryGetValue tdef with
            | true, members when members.ContainsMethod ctor -> Ok ctor.ParameterTypes.Length
            | true, _ -> Error(noImpl "error for defined attribute ctor not found")
            | false, _ -> Error(noImpl "error for defined attribute type not found")
        | CustomAttributeCtor.Ref(MethodCallTarget(tref, ctor)) ->
            match referencedTypes.TryGetValue tref with
            | true, members when members.ContainsMethod ctor -> Ok ctor.ParameterTypes.Length
            | true, _ -> Error(noImpl "error for referenced attribute ctor not found")
            | false, _ -> Error(noImpl "error for referenced attribute type not found")

    let customAttributeLookup = CustomAttribute.LookupBuilder customAttributeResolver

    let createAttributeList owner = CustomAttributeList(owner, customAttributeLookup)
    let defineMembersFor owner = DefinedTypeMembers(owner, warnings, entryPointToken, customAttributeLookup)

    let mutable assemblyDefAttributes =
        match assembly with
        | Some assembly' -> Some(createAttributeList(CustomAttribute.Owner.DefinedAssembly assembly'))
        | None -> None

    member val ValidationWarnings = ValidationWarningsCollection(?warnings = warnings)
    member val Mvid = Option.defaultWith Guid.NewGuid mvid
    member val ModuleCustomAttributes = createAttributeList(CustomAttribute.Owner.DefinedType ModuleType)
    member val GlobalMembers = defineMembersFor ModuleType
    member _.Name: Identifier = name
    member _.EntryPoint = !entryPointToken
    member _.Assembly = assemblyDef
    member _.AssemblyCustomAttributes = assemblyDefAttributes

    member this.DefineAssembly(assembly: DefinedAssembly) =
        match assemblyDef with
        | Some existing -> ValidationResult.Error(failwith "TODO: Warning for duplicate assembly definition")
        | None ->
            let attrs = createAttributeList(CustomAttribute.Owner.DefinedAssembly assembly)
            assemblyDef <- Some assembly
            assemblyDefAttributes <- Some attrs
            Ok attrs

    member _.ReferenceAssembly assembly =
        if not (assemblyRefs.Add assembly) then
            match warnings with
            | Some warnings' -> warnings'.Add(failwith "TODO: Warning for duplicate assembly reference")
            | None -> ()

    member private _.AddDefinedType tdef = // TODO: Prevent addition of type named <Module>
        match definedTypes.TryGetValue tdef with
        | true, existing -> ValidationResult<_>.Error(noImpl "TODO: error for duplicate type def")
        | false, _ ->
            // TODO: Check that extends and nestedclass are already contained in the Module.
            let members = defineMembersFor tdef
            definedTypes.[tdef] <- members
            genericParameterLists.[tdef] <- tdef.GenericParameters
            Ok members

    member this.DefineType definition =
        validated {
            let! members = this.AddDefinedType definition
            return struct(createAttributeList(CustomAttribute.Owner.DefinedType definition), members)
        }

    member this.DefineType(definition, attributes) =
        validated {
            let! members = this.AddDefinedType definition
            CustomAttributeList.fromRef (CustomAttribute.Owner.DefinedType definition) customAttributeLookup attributes
            return members
        }

    member _.ReferenceType reference =
        match referencedTypes.TryGetValue reference with
        | true, existing -> ValidationResult<_>.Error(noImpl "TODO: error for duplicate type ref")
        | false, _ ->
            // TODO: Check that the resolution scope is already accounted for.
            let members = ReferencedTypeMembers(reference, warnings)
            referencedTypes.[reference] <- members
            Ok members

    member this.ReferenceType(reference: TypeReference<'Kind>) =
        validated {
            let! members = this.ReferenceType(reference :> TypeReference)
            return ReferencedTypeMembers<'Kind> members
        }

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

    member internal this.Serialize() =
        let serializer =
            ModuleBuilderSerializer (
                cliMetadataHeader',
                cliMetadataRoot',
                name,
                this.Mvid,
                customAttributeLookup,
                genericParameterLists,
                assembly,
                this.EntryPoint,
                assemblyRefs,
                definedTypes,
                this.GlobalMembers,
                referencedTypes
            )
        serializer.Serialize()
