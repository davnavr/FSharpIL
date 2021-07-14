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
    | EntryPointMethod of DefinedType * EntryPointMethod

[<RequireQualifiedAccess>]
module EntryPoint =
    let (|None|Method|) entryPoint =
        match entryPoint with
        | NoEntryPoint -> None
        | EntryPointMethod(owner, method) -> Method(struct(owner, method))

[<Sealed>]
type DefinedTypeMembers =
    val private owner: DefinedType
    val private warnings: ValidationWarningsBuilder option
    val private entryPointToken: EntryPoint ref
    [<DefaultValue>] val mutable internal Field: HybridHashSet<DefinedField>
    [<DefaultValue>] val mutable Method: HybridHashSet<DefinedMethod>
    [<DefaultValue>] val mutable MethodBodyLookup: LateInitDictionary<DefinedMethod, DefinedMethodBody>

    new (owner, warnings, entryPointToken) = { owner = owner; warnings = warnings; entryPointToken = entryPointToken }

    member this.FieldCount = this.Field.Count
    member this.MethodCount = this.Method.Count

[<Sealed>]
type ReferencedTypeMembers = class
    val private owner: ReferencedType
    val private warnings: ValidationWarningsBuilder option
    [<DefaultValue>] val mutable Field: HybridHashSet<ReferencedField>
    [<DefaultValue>] val mutable Method: HybridHashSet<ReferencedMethod>

    new (owner, warnings) = { owner = owner; warnings = warnings }

    member this.FieldCount = this.Field.Count
    member this.MethodCount = this.Method.Count
end

[<IsReadOnly>]
[<NoComparison; NoEquality>]
type ConstructedCustomAttribute = struct
    val Constructor: CustomAttributeCtor
    val Signature: CustomAttrib

    new (ctor, fixedArgs, namedArgs) =
        { Constructor = ctor
          Signature = { FixedArgs = fixedArgs; NamedArgs = namedArgs } }
end

[<Sealed>]
type CustomAttributeList
    (
        owner: obj,
        lookup: Dictionary<obj, _>,
        resolver: CustomAttributeCtor -> ValidationResult<int32>
    ) =
    let mutable attributes = Unchecked.defaultof<ImmutableArray<ConstructedCustomAttribute>.Builder>

    member _.Count = if isNull attributes then 0 else attributes.Count

    static member private CreateFixedArgsLoop
        (
            fixedArgs: byref<FixedArg[]>,
            parameterTypes: ImmutableArray<Signatures.ParamItem>,
            parameterNames: ImmutableArray<Parameter>,
            fixedArgsSource: FixedArgSource,
            i
        )
        =
        if i < fixedArgs.Length then
            let paramName =
                if parameterNames.IsDefaultOrEmpty
                then ValueNone
                else parameterNames.ItemRef(i).ParamName

            let fixedArgType =
                let paramType = &parameterTypes.ItemRef i
                match ValueOption.bind EncodedType.toElemType paramType.ParamType with
                | ValueSome etype -> Ok etype
                | _ -> Error(noImpl "error for invalid custom attribute argument type")

            match fixedArgType with
            | Ok fixedArgType' ->
                match fixedArgsSource i paramName fixedArgType' with
                | Ok fixedArg ->
                    fixedArgs.[i] <- fixedArg
                    CustomAttributeList.CreateFixedArgsLoop(&fixedArgs, parameterTypes, parameterNames, fixedArgsSource, i + 1)
                | Error(ValueSome err) -> Error err
                | Error ValueNone -> Error(noImpl "error for no argument provided to custom attribute constructor")
            | Error err -> Error err
        else Ok(Unsafe.As<_, ImmutableArray<FixedArg>> &fixedArgs)

    static member private CreateFixedArgs(fixedArgsSource, numFixedArgs, customAttribCtor) =
        let mutable fixedArgs = Array.zeroCreate numFixedArgs

        let parameterTypes, parameterNames =
            match customAttribCtor with
            | CustomAttributeCtor.Ref(_, ctor) -> ctor.ParameterTypes, ImmutableArray.Empty
            | CustomAttributeCtor.Def(_, ctor) -> ctor.ParameterTypes, ctor.Parameters

        CustomAttributeList.CreateFixedArgsLoop(&fixedArgs, parameterTypes, parameterNames, fixedArgsSource, 0)

    member _.Add attrib =
        validated {
            let! numFixedArgs = resolver attrib.Constructor
            let! fixedArguments = CustomAttributeList.CreateFixedArgs(attrib.FixedArguments, numFixedArgs, attrib.Constructor)

            if isNull attributes then
                attributes <- ImmutableArray.CreateBuilder()
                lookup.[owner] <- attributes

            attributes.Add(ConstructedCustomAttribute(attrib.Constructor, fixedArguments, attrib.NamedArguments))
        }

[<IsReadOnly; Struct>]
type ReferencedTypeEntry =
    { TypeRef: TableIndex<TypeRefRow>
      Members: Dictionary<obj, TableIndex<MemberRefRow>> }

[<NoComparison; NoEquality>]
type DefinedTypeEntry =
    { TypeDef: TableIndex<TypeDefRow>
      Fields: Dictionary<DefinedField, TableIndex<FieldRow>>
      Methods: Dictionary<DefinedMethod, TableIndex<MethodDefRow>> }

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

type ModuleBuilderSerializer
    (
        header,
        root,
        moduleBuilderObj: obj,
        name: Identifier,
        mvid,
        attributes: Dictionary<_, _>,
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
            header,
            root,
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
    let definedTypeLookup = Dictionary<DefinedType, _> definedTypes.Count

    let fieldSignatures = Dictionary<_, FieldSigOffset>((definedTypeLookup.Count + referencedTypes.Count) * 8)
    let methodDefSignatures = Dictionary<MethodDefSig, MethodDefSigOffset>(definedTypeLookup.Count * 16)
    let methodRefSignatures = Dictionary<MethodRefSig, MemberRefSigOffset>(referencedTypes.Count * 32) // TODO: Helper function for getting and adding signatures.
    //let localVarSignatures = Dictionary<Signatures.LocalVarSig, TableIndex<StandaloneSigRow>>(methodDefSignatures.Count)

    let mutable methodDefParams = TableIndex<ParamRow>.One

    // TODO: Fix, a typedef with no members may mess up the member list (previous type has one member missing).
    let mutable indices =
        { FieldList = TableIndex.One
          MethodList = TableIndex.One
          EventList = TableIndex.One
          PropertyList = TableIndex.One }

    let namedTypeMapping =
        function
        | DefinedType tdef ->
            MetadataSignatures.TypeDefOrRefEncoded.Def(serializer.SerializeDefinedType(tdef, definedTypes.[tdef]))
        | ReferencedType tref ->
            MetadataSignatures.TypeDefOrRefEncoded.Ref(serializer.SerializeReferencedType(tref, referencedTypes.[tref]))

    let modifierTypeMapping =
        function
        | TypeDefOrRefOrSpec.Def tdef -> TypeDefOrRef.Def(serializer.SerializeDefinedType(tdef, definedTypes.[tdef]))
        | TypeDefOrRefOrSpec.Ref tref -> TypeDefOrRef.Ref(serializer.SerializeReferencedType(tref, referencedTypes.[tref]))
        | TypeDefOrRefOrSpec.Spec tspec -> TypeDefOrRef.Spec(serializer.SerializeTypeSpec tspec)

    let localVarSource (signature: LocalVarSig) =
        if signature.IsDefaultOrEmpty
        then Unchecked.defaultof<_>
        else
            // TODO: Consider caching local variable signatures in localVarSignatures dictionary.
            Blob.mapLocalVarSig namedTypeMapping modifierTypeMapping signature
            |> failwith "TODO: write locals to blob stream"

    let methodTokenSource = { MethodCalls = ImmutableArray.CreateBuilder(definedTypes.Count * 64) }
    let fieldTokenSource = { FieldInstructions = ImmutableArray.CreateBuilder methodTokenSource.MethodCalls.Capacity }
    let typeTokenSource =
        { TypeInstructions = ImmutableArray.CreateBuilder(definedTypes.Count + referencedTypes.Count + typeSpecSet.Count) }

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

    member this.Serialize() =
        this.SerializeAssemblyReferences()

        for KeyValue(tref, members) in referencedTypes do this.SerializeReferencedType(tref, members) |> ignore

        // Write the special <Module> type.
        this.SerializeDefinedType(ModuleType, moduleGlobalMembers) |> ignore

        for KeyValue(tdef, members) in definedTypes do this.SerializeDefinedType(tdef, members) |> ignore

        builder.EntryPointToken <-
            match entryPointToken with
            | NoEntryPoint -> EntryPointToken.Null
            | EntryPointMethod(owner, method) -> EntryPointToken.MethodDef definedTypeLookup.[owner].Methods.[method.Method]
            //| EntryPointFile file -> EntryPointToken.File(failwith "TODO: get entry point file")

        this.SerializeGenericParams()
        this.SerializeCustomAttributes()
        this.PatchMethodCalls()
        this.PatchFieldInstructions()
        this.PatchTypeInstructions()

        builder

[<Sealed>]
type CliModuleBuilder // TODO: Consider making an immutable version of this class.
    (
        name,
        ?mvid,
        ?header,
        ?root,
        ?assembly: DefinedAssembly,
        ?warnings,
        ?typeDefCapacity,
        ?typeRefCapacity,
        ?typeSpecCapacity,
        ?assemblyRefCapacity
    )
    as builder
    =
    let header' = defaultArg header CliHeader.latestDefault
    let root' = defaultArg root CliMetadataRoot.defaultFields
    let assemblyRefs = HashSet<ReferencedAssembly>(defaultArg assemblyRefCapacity 8)
    let definedTypes = Dictionary<DefinedType, DefinedTypeMembers>(defaultArg typeDefCapacity 16)
    let referencedTypes = Dictionary<ReferencedType, ReferencedTypeMembers>(defaultArg typeRefCapacity 32)
    let attributes = Dictionary<obj, ImmutableArray<_>.Builder> 8
    let gparams = Dictionary<obj, GenericParamList> 16
    let entryPointToken = ref Unchecked.defaultof<EntryPoint>

    let customAttributeResolver =
        function
        | CustomAttributeCtor.Def(tdef, ctor) ->
            match definedTypes.TryGetValue tdef with
            | true, members when members.ContainsMethod ctor -> Ok ctor.ParameterTypes.Length
            | true, _ -> Error(noImpl "error for defined attribute ctor not found")
            | false, _ -> Error(noImpl "error for defined attribute type not found")
        | CustomAttributeCtor.Ref(tref, ctor) ->
            match referencedTypes.TryGetValue tref with
            | true, members when members.ContainsMethod ctor -> Ok ctor.ParameterTypes.Length
            | true, _ -> Error(noImpl "error for referenced attribute ctor not found")
            | false, _ -> Error(noImpl "error for referenced attribute type not found")

    member val AssemblyCustomAttributes =
        match assembly with
        | Some assembly' -> Some(CustomAttributeList(assembly', attributes, customAttributeResolver))
        | None -> None

    member val UserStrings = UserStringStreamBuilder 1
    member val ValidationWarnings = ValidationWarningsCollection(?warnings = warnings)
    member val Mvid = Option.defaultWith Guid.NewGuid mvid
    member val ModuleCustomAttributes = CustomAttributeList(builder, attributes, customAttributeResolver)
    member val GlobalMembers = DefinedTypeMembers(ModuleType, warnings, entryPointToken)
    member _.Name: Identifier = name
    member _.Assembly = assembly
    member _.EntryPoint = !entryPointToken

    member _.DefinedTypes = definedTypes.Keys :> IReadOnlyCollection<_>
    member _.ReferencedTypes = referencedTypes.Keys :> IReadOnlyCollection<_>
    member _.ReferencedAssemblies = assemblyRefs :> IReadOnlyCollection<_>

    member inline private _.CreateAttributeList owner = CustomAttributeList(owner, attributes, customAttributeResolver)

    member _.ReferenceAssembly assem =
        if not (assemblyRefs.Add assem) then
            match warnings with
            | Some warnings' -> warnings'.Add(failwith "TODO: Warning for duplicate assembly reference")
            | None -> ()

    member internal this.Serialize() =
        let serializer =
            ModuleBuilderSerializer (
                header',
                root',
                this,
                name,
                this.Mvid,
                attributes,
                gparams,
                this.UserStrings,
                assembly,
                this.EntryPoint,
                assemblyRefs,
                definedTypes,
                this.GlobalMembers,
                referencedTypes
            )
        serializer.Serialize()
