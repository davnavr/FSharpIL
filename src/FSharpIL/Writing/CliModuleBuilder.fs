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


[<IsReadOnly>]
[<NoComparison; NoEquality>]
type ConstructedCustomAttribute = struct
    val Constructor: CustomAttributeCtor
    val Signature: CustomAttrib

    new (ctor, fixedArgs, namedArgs) =
        { Constructor = ctor
          Signature = { FixedArgs = fixedArgs; NamedArgs = namedArgs } }
end

/// Returns the number of fixed arguments from the specified constructor method.
type CustomAttributeResolver = CustomAttributeCtor -> ValidationResult<int32>

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type CustomAttributeOwner =
    { Owner: obj }

    interface IEquatable<CustomAttributeOwner> with member this.Equals other = this.Owner.Equals other.Owner

(*
type CustomAttributeOwner =
    (CliModuleBuilder
    |DefinedAssembly
    |DefinedType)
*)

[<Sealed>]
type CustomAttributeList // TODO: Make this a struct somehow.
    (
        owner: CustomAttributeOwner,
        lookup: Dictionary<CustomAttributeOwner, _>,
        resolver: CustomAttributeResolver
    )
    =
    let mutable attributes = Unchecked.defaultof<ImmutableArray<ConstructedCustomAttribute>.Builder>

    member _.Count = if isNull attributes then 0 else attributes.Count

    static member private CreateFixedArgsLoop
        (
            fixedArgs: byref<FixedArg[]>,
            parameterTypes: ImmutableArray<MethodParameterType>,
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

            match parameterTypes.[i] with
            | MethodParameterType.Type t ->
                match NamedType.toElemType t with
                | ValueSome etype ->
                    match fixedArgsSource i paramName etype with
                    | Ok fixedArg ->
                        fixedArgs.[i] <- fixedArg
                        CustomAttributeList.CreateFixedArgsLoop(&fixedArgs, parameterTypes, parameterNames, fixedArgsSource, i + 1)
                    | Error(ValueSome err) -> Error err
                    | Error ValueNone -> Error(noImpl "error for no argument provided to custom attribute constructor")
                | ValueNone -> Error(noImpl "error for invalid custom attribute argument type")
            | _ -> Error(noImpl "error for custom attribute argument type cannot be byref type")
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

    member this.AddDefinedMethod(method, body: DefinedMethodBody voption) =
        // TODO: Check that owner is the correct kind of type to own this kind of method.
        if this.Method.Add method then
            match body with
            | ValueSome body' -> this.MethodBodyLookup.[method] <- body'
            | ValueNone -> ()

            Ok(MethodCallTarget.Defined(this.owner, method))
        else ValidationResult.Error(noImpl "error for duplicate method")

    member this.AddDefinedMethod(implFlags, flags, methodThis, returnType, name, parameterTypes, parameterList, body) =
        this.AddDefinedMethod(DefinedMethod(implFlags, flags, methodThis, returnType, name, parameterTypes, parameterList), body)

    member this.DefineMethod(implFlags, flags, methodThis, returnType, name, parameterTypes, parameterList, body, attributes) =
        validated {
            let! target = this.AddDefinedMethod(implFlags, flags, methodThis, returnType, name, parameterTypes, parameterList, body)

            // TODO: Create helepr function for creating custom attribute lists.
            match attributes with
            | ValueSome attributes' -> attributes' := (failwith "TODO: How to get access to custom attribute list maker from here.": CustomAttributeList)
            | ValueNone -> ()

            return target
        }

    member this.ContainsField field = this.Field.Contains field
    member this.ContainsMethod method = this.Method.Contains method

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

[<System.Obsolete>]
type IBlobLookup<'Sig, 'Blob> = interface
    abstract Write: 'Sig * BlobStreamBuilder -> 'Blob
end

[<System.Obsolete>]
type FieldSignatureLookup = struct
    interface IBlobLookup<Field, FieldSigOffset> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.Write(field, blobs) =
            failwith "bad" // TODO: How to get access to lookup for typeRef and typeDef?
end

[<System.Obsolete>]
[<Struct>]
type BlobSignatureLookup<'Lookup, 'Sig, 'Blob
    when 'Lookup :> IBlobLookup<'Sig, 'Blob>
    and 'Lookup : struct
    and 'Sig : equality
    and 'Blob : struct>
    =
    val mutable private signatures: LateInitDictionary<'Sig, 'Blob>
    //val something: 'Sig -> 'Blob

    member this.GetOrAdd(blobs, signature) =
        match this.signatures.TryGetValue signature with
        | true, existing -> existing
        | false, _ ->
            let blob = Unchecked.defaultof<'Lookup>.Write(signature, blobs)
            this.signatures.[signature] <- blob
            blob

type ModuleBuilderSerializer
    (
        cliMetadataHeader,
        cliMetadataRoot,
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
    let definedTypeLookup = Dictionary<DefinedType, _> definedTypes.Count

    let mutable fieldSigLookup = BlobSignatureLookup<FieldSignatureLookup, _, _>()
    let methodSigLookup = Dictionary<Method, _>(64, Method.comparer)

    let mutable methodDefParams = TableIndex<ParamRow>.One

    // TODO: Fix, a typedef with no members may mess up the member list (previous type has one member missing).
    let mutable indices =
        { FieldList = TableIndex.One
          MethodList = TableIndex.One
          EventList = TableIndex.One
          PropertyList = TableIndex.One }

    let methodTokenSource = { MethodCalls = ImmutableArray.CreateBuilder(definedTypes.Count * 64) }
    let fieldTokenSource = { FieldInstructions = ImmutableArray.CreateBuilder methodTokenSource.MethodCalls.Capacity }

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

    member private _.GetMethodSig method =
        match methodSigLookup.TryGetValue method with
        | true, existing -> existing
        | false, _ ->
            let blob = failwith "TODO: Get method signature"
            methodSigLookup.[method] <- blob
            blob

    member private this.SerializeReferencedType(tref, members) =
        match referencedTypeLookup.TryGetValue tref with
        | true, { TypeRef = i } -> i
        | false, _ ->
            let rscope =
                match tref.ResolutionScope with
                | TypeReferenceParent.Null -> ResolutionScope.Null
                | TypeReferenceParent.Assembly assem -> ResolutionScope.AssemblyRef assemblyReferenceLookup.[assem]
                | TypeReferenceParent.Type parent ->
                    ResolutionScope.TypeRef(this.SerializeReferencedType(parent, referencedTypes.[parent]))

            let i =
                builder.Tables.TypeRef.Add
                    { ResolutionScope = rscope
                      TypeName = builder.Strings.Add tref.TypeName
                      TypeNamespace = builder.Strings.Add tref.TypeNamespace }

            let members' = Dictionary<obj, TableIndex<MemberRefRow>>(members.FieldCount + members.MethodCount)

            for field in members.Field do
                members'.[field] <-
                    builder.Tables.MemberRef.Add
                        { Class = MemberRefParent.TypeRef i
                          Name = builder.Strings.Add field.Name
                          Signature = { MemberRefSig = fieldSigLookup.GetOrAdd(builder.Blob, field).FieldSig } }

            for method in members.Method do
                members'.[method] <-
                    builder.Tables.MemberRef.Add
                        { Class = MemberRefParent.TypeRef i
                          Name = builder.Strings.Add method.Name
                          Signature =
                            // TODO: Use separate lookup if method reference signature has any VarArgs.
                            { MemberRefSig = this.GetMethodSig(method).MethodDefSig } }

            referencedTypeLookup.[tref] <- { TypeRef = i; Members = members' }
            i

    member this.Serialize() =
        this.SerializeAssemblyReferences()

        for KeyValue(tref, members) in referencedTypes do this.SerializeReferencedType(tref, members) |> ignore

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
    let attributes = Dictionary<CustomAttributeOwner, ImmutableArray<_>.Builder> 8
    let genericParameterLists = Dictionary<obj, GenericParamList> 16 // TODO: Make a GenericParamOwner struct.
    let entryPointToken = ref Unchecked.defaultof<EntryPoint>

    let customAttributeResolver =
        function
        | CustomAttributeCtor.Def(tdef, ctor) ->
            match definedTypes.TryGetValue tdef with
            //| true, members when members.ContainsMethod ctor -> Ok ctor.ParameterTypes.Length
            | true, _ -> Error(noImpl "error for defined attribute ctor not found")
            | false, _ -> Error(noImpl "error for defined attribute type not found")
        | CustomAttributeCtor.Ref(tref, ctor) ->
            match referencedTypes.TryGetValue tref with
            //| true, members when members.ContainsMethod ctor -> Ok ctor.ParameterTypes.Length
            | true, _ -> Error(noImpl "error for referenced attribute ctor not found")
            | false, _ -> Error(noImpl "error for referenced attribute type not found")

    member val UserStrings = UserStringStreamBuilder 1
    member val ValidationWarnings = ValidationWarningsCollection(?warnings = warnings)
    member val Mvid = Option.defaultWith Guid.NewGuid mvid
    member val ModuleCustomAttributes = CustomAttributeList({ Owner = builder }, attributes, customAttributeResolver)
    member val GlobalMembers = DefinedTypeMembers(ModuleType, warnings, entryPointToken)
    member _.Name: Identifier = name
    member _.EntryPoint = !entryPointToken
    member _.Assembly = assemblyDef
    member _.AssemblyCustomAttributes = assemblyDefAttributes

    member inline private _.CreateAttributeList owner =
        CustomAttributeList({ Owner = owner }, attributes, customAttributeResolver)

    member this.DefineAssembly assembly =
        match assemblyDef with
        | Some existing -> ValidationResult.Error(failwith "TODO: Warning for duplicate assembly definition")
        | None ->
            let attrs = this.CreateAttributeList assembly
            assemblyDef <- Some assembly
            assemblyDefAttributes <- Some attrs
            Ok attrs

    member _.ReferenceAssembly assembly =
        if not (assemblyRefs.Add assembly) then
            match warnings with
            | Some warnings' -> warnings'.Add(failwith "TODO: Warning for duplicate assembly reference")
            | None -> ()

    member private _.AddDefinedType(flags, extends, tnamespace, enclosing, tname, gparameters) =
        let tdef = DefinedType(flags, extends, tnamespace, enclosing, tname, gparameters)
        match definedTypes.TryGetValue tdef with
        | true, existing -> ValidationResult<_>.Error(noImpl "TODO: error for duplicate type def")
        | false, _ ->
            // TODO: Check that extends and nestedclass are already contained in the Module.
            let members = DefinedTypeMembers(tdef, warnings, entryPointToken)
            definedTypes.[tdef] <- members
            genericParameterLists.[tdef] <- tdef.GenericParameters
            Ok(struct(tdef, members))

    member this.DefineType(flags, extends, typeNamespace, enclosingClass, typeName, genericParameters) =
        validated {
            let! struct(tdef, members) =
                this.AddDefinedType(flags, extends, typeNamespace, enclosingClass, typeName, genericParameters)
            return struct(this.CreateAttributeList tdef, members)
        }

    member this.DefineType(flags, extends, typeNamespace, enclosingClass, typeName, genericParameters, attributes) =
        validated {
            let! struct(tdef, members) =
                this.AddDefinedType(flags, extends, typeNamespace, enclosingClass, typeName, genericParameters)

            match attributes with
            | ValueSome attributes' -> attributes' := this.CreateAttributeList tdef
            | ValueNone -> ()

            return members
        }

    member internal this.Serialize() =
        let serializer =
            ModuleBuilderSerializer (
                cliMetadataHeader',
                cliMetadataRoot',
                this,
                name,
                this.Mvid,
                attributes,
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
