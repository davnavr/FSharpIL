namespace FSharpIL.Writing

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

[<AbstractClass>]
type DefinedMethodBody =
    val LocalTypes: LocalVarSig
    val InitLocals: InitLocals

    new (localTypes, initLocals) = { LocalTypes = localTypes; InitLocals = initLocals }
    new (localTypes) = DefinedMethodBody(localTypes, SkipInitLocals)
    new () = DefinedMethodBody ImmutableArray.Empty

    /// Writes the instructions of the method body, and returns the maximum number of items on the stack.
    abstract WriteInstructions: byref<MethodBodyBuilder> * MethodTokenSource * FieldTokenSource * TypeTokenSource -> uint16

type EntryPoint =
    private
    | NoEntryPoint
    | EntryPointMethod of MethodCallTarget<DefinedType, MethodDefinition<MethodKinds.Static>>

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
        private { Owner: obj }
        interface IEquatable<Owner> with member this.Equals other = this.Owner.Equals other.Owner

        static member DefinedAssembly (assembly: DefinedAssembly) = { Owner = assembly }
        static member DefinedType (tdef: DefinedType) = { Owner = tdef }
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
type DefinedTypeMembers (owner: DefinedType, warnings: ValidationWarningsBuilder option, entryPointToken: EntryPoint ref, attrs) =
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

            Ok(MethodCallTarget<DefinedType, DefinedMethod>(owner, method))
        else ValidationResult.Error(noImpl "error for duplicate method")

    member this.DefineMethod(method, body, attributes) =
        validated {
            let! target = this.AddDefinedMethod(method, body)
            CustomAttributeList.fromRef (CustomAttribute.Owner.DefinedMethod method) attrs attributes
            return target
        }

    member this.ContainsField field = this.Field.Contains field
    member this.ContainsMethod method = this.Method.Contains method

[<Sealed>]
type ReferencedTypeMembers =
    val private owner: ReferencedType
    val private warnings: ValidationWarningsBuilder option
    [<DefaultValue>] val mutable Field: HybridHashSet<ReferencedField>
    [<DefaultValue>] val mutable Method: HybridHashSet<ReferencedMethod>

    new (owner, warnings) = { owner = owner; warnings = warnings }

    member this.FieldCount = this.Field.Count
    member this.MethodCount = this.Method.Count

    member this.ReferenceMethod(method: ReferencedMethod) =
        // TODO: Check that the kind and owner of method are correct.
        if this.Method.Add method
        then ValidationResult.Ok(MethodCallTarget<ReferencedType, ReferencedMethod>(this.owner, method))
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
        localVarSource: LocalVarSig -> TableIndex<StandaloneSigRow>,
        methodTokenSource: MethodTokenSource,
        fieldTokenSource: FieldTokenSource,
        typeTokenSource: TypeTokenSource,
        body: DefinedMethodBody
    )
    =
    interface IMethodBodySource with
        member _.Create builder =
            { InitLocals = body.InitLocals
              MaxStack = body.WriteInstructions(&builder, methodTokenSource, fieldTokenSource, typeTokenSource)
              LocalVariables = localVarSource body.LocalTypes }

[<AutoOpen>]
module Helpers =
    type Dictionary<'Key, 'Value> with
        member inline this.EnsureAdditionalCapacity additional = this.EnsureCapacity(this.Count + additional)

type ModuleBuilderSerializer
    (
        cliMetadataHeader,
        cliMetadataRoot,
        moduleBuilderObj: obj,
        name: Identifier,
        mvid,
        attributes: CustomAttribute.LookupBuilder,
        gparams: Dictionary<_, _>,
        userStringStream,
        assembly: DefinedAssembly option,
        entryPointToken: EntryPoint,
        assemblyReferences: HashSet<ReferencedAssembly>,
        definedTypes: Dictionary<DefinedType, DefinedTypeMembers>,
        moduleGlobalMembers: DefinedTypeMembers,
        referencedTypes: Dictionary<ReferencedType, ReferencedTypeMembers>
    )
    as serializer
    =
    let builder =
        let strings = StringsStreamBuilder 1024
        let guids = GuidStreamBuilder 1
        let blobs = BlobStreamBuilder 512
        CliMetadataBuilder (
            cliMetadataHeader,
            cliMetadataRoot,
            FSharpIL.Writing.Cil.MethodBodyList(),
            MetadataTablesBuilder((fun str guid _ -> ModuleRow.create (str.Add name) (guid.Add mvid)), strings, guids, blobs),
            strings,
            userStringStream,
            guids,
            blobs
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

    let referencedTypeLookup = Dictionary<ReferencedType, _> referencedTypes.Count
    let referencedFieldLookup = Dictionary<FieldArg<ReferencedType, ReferencedField>, _>()
    let referencedMethodLookup = Dictionary<MethodCallTarget<ReferencedType, ReferencedMethod>, _>()

    let definedTypeLookup = Dictionary<DefinedType, _> definedTypes.Count
    let definedFieldLookup = Dictionary<FieldArg<DefinedType, DefinedField>, _>()
    let definedMethodLookup = Dictionary<MethodCallTarget<DefinedType, DefinedMethod>, _>()

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
        | :? DefinedType as tdef -> typeDefEncoded tdef
        | :? ReferencedType as tref -> typeRefEncoded tref
        | bad -> invalidOp(sprintf "Expected defined or referenced type but got %A" bad)

    let rec typeDefOrRefOrSpec: NamedType -> _ =
        function
        | :? DefinedType as tdef -> TypeDefOrRef.Def(serializer.SerializeDefinedType(tdef, definedTypes.[tdef]))
        | :? ReferencedType as tref -> TypeDefOrRef.Ref(serializer.SerializeReferencedType tref)
        | tspec ->  TypeDefOrRef.Spec(getTypeSpec(getEncodedType tspec))

    and getTypeSpec =
        createBlobLookup EqualityComparer.Default <| fun (tspec: EncodedType) ->
            builder.Tables.TypeSpec.Add { TypeSpec = builder.Blob.Add tspec }

    // TODO: How to decide if VALUETYPE or CLASS should be used? Maybe make a ClassType and ValueType derived from NamedType while also checking if a DefinedType is derived from ValueType or Enum
    and getEncodedType (t: NamedType) =
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
            let rec inner i modifiers =
                if i >= 0 then
                    let modf' =  modf.Modifiers.[i]
                    inner
                        (i - 1)
                        ({ CustomMod.Required = modf'.Required; ModifierType = typeDefOrRefOrSpec modf'.Modifier } :: modifiers)
                else modifiers
            EncodedType.Modified(inner (modf.Modifiers.Length - 1) List.empty, getEncodedType modf.Modified)
        | :? ModifiedType as modf -> getEncodedType modf.Modified
        | _ -> invalidOp(sprintf "Cannot encode unsupported type %s" (t.GetType().Name))

    let getMethodSig =
        createBlobLookup Method.signatureComparer <| fun (method: Method) ->
            failwith "TODO: Get method signature"

    let getFieldSig =
        createBlobLookup Field.signatureComparer <| fun (field: Field) ->
            let signature = { FieldType = getEncodedType field.Type }
            builder.Blob.Add &signature

    let mutable methodDefParams = TableIndex<ParamRow>.One

    // TODO: Fix, a typedef with no members may mess up the member list (previous type has one member missing).
    let mutable indices =
        { FieldList = TableIndex.One
          MethodList = TableIndex.One
          EventList = TableIndex.One
          PropertyList = TableIndex.One }

    let methodTokenSource = { MethodCalls = ImmutableArray.CreateBuilder() }
    let fieldTokenSource = { FieldInstructions = ImmutableArray.CreateBuilder() }
    let typeTokenSource = { TypeInstructions = ImmutableArray.CreateBuilder() }

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

    member private this.SerializeReferencedType(reference: ReferencedType) =
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
                                DefinedMethodBodyWriter (
                                    noImpl "TODO: Figure out how to get local var info",
                                    methodTokenSource,
                                    fieldTokenSource,
                                    typeTokenSource,
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

            for KeyValue(parent, attrs) in attributes do
                let index =
                    if Object.ReferenceEquals(parent, moduleBuilderObj) then
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

        this.SerializeCustomAttributes()

        // PATCHES

        builder

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
    as builder
    =
    let cliMetadataHeader' = defaultArg cliMetadataHeader CliHeader.latestDefault
    let cliMetadataRoot' = defaultArg cliMetadataRoot CliMetadataRoot.defaultFields
    let mutable assemblyDef, assemblyDefAttributes = assembly, None
    let assemblyRefs = HashSet<ReferencedAssembly>(defaultArg assemblyRefCapacity 8)
    let definedTypes = Dictionary<DefinedType, DefinedTypeMembers>(defaultArg typeDefCapacity 16)
    let referencedTypes = Dictionary<ReferencedType, ReferencedTypeMembers>(defaultArg typeRefCapacity 32)
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
            //| true, members when members.ContainsMethod ctor -> Ok ctor.ParameterTypes.Length
            | true, _ -> Error(noImpl "error for referenced attribute ctor not found")
            | false, _ -> Error(noImpl "error for referenced attribute type not found")

    let customAttributeLookup = CustomAttribute.LookupBuilder customAttributeResolver

    let createAttributeList owner = CustomAttributeList(owner, customAttributeLookup)
    let defineMembersFor owner = DefinedTypeMembers(owner, warnings, entryPointToken, customAttributeLookup)

    member val UserStrings = UserStringStreamBuilder 1
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

    member private _.AddDefinedType tdef =
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

    member this.ReferenceType reference =
        match referencedTypes.TryGetValue reference with
        | true, existing -> ValidationResult<_>.Error(noImpl "TODO: error for duplicate type ref")
        | false, _ ->
            // TODO: Check that the resolution scope is already accounted for.
            let members = ReferencedTypeMembers(reference, warnings)
            referencedTypes.[reference] <- members
            Ok members

    member internal this.Serialize() =
        let serializer =
            ModuleBuilderSerializer (
                cliMetadataHeader',
                cliMetadataRoot',
                this,
                name,
                this.Mvid,
                customAttributeLookup,
                genericParameterLists,
                this.UserStrings,
                assembly,
                this.EntryPoint,
                assemblyRefs,
                definedTypes,
                this.GlobalMembers,
                referencedTypes
            )
        serializer.Serialize()
