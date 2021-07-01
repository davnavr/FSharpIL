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

[<AbstractClass>]
type DefinedMethodBody =
    val LocalTypes: Signatures.LocalVarSig
    val InitLocals: InitLocals

    new (localTypes, initLocals) = { LocalTypes = localTypes; InitLocals = initLocals }
    new (localTypes) = DefinedMethodBody(localTypes, SkipInitLocals)
    new () = DefinedMethodBody ImmutableArray.Empty

    /// Writes the instructions of the method body, and returns the maximum number of items on the stack.
    abstract WriteInstructions: byref<MethodBodyBuilder> * MethodTokenSource -> uint16

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
    [<DefaultValue>] val mutable Method: HybridHashSet<DefinedMethod>
    [<DefaultValue>] val mutable MethodBodyLookup: LateInitDictionary<DefinedMethod, DefinedMethodBody>

    new (owner, warnings, entryPointToken) = { owner = owner; warnings = warnings; entryPointToken = entryPointToken }

    member this.MethodCount = this.Method.Count

    member this.AddMethod(method: DefinedMethod, body: DefinedMethodBody voption) =
        match this.owner with
        | :? TypeDefinition<TypeKinds.StaticClass> ->
            match method with
            | :? MethodDefinition<MethodKinds.Static> ->
                match this.Method.Add method, body with
                | true, ValueSome body' ->
                    this.MethodBodyLookup.[method] <- body'
                    ValidationResult.Ok(MethodCall.Defined(this.owner, method))
                | true, ValueNone -> noImpl "error for missing method body"
                | false, _ -> noImpl "error for duplicate method"
            | _ -> noImpl "what method definition"
        | _ -> noImpl "no handler for adding method to this defined type"

    member this.AddEntryPoint(method: EntryPointMethod, body) =
        match this.AddMethod(method.Method, ValueSome body) with
        | Ok result ->
            this.entryPointToken := EntryPointMethod(this.owner, method)
            Ok result
        | Error err -> Error err

    member this.ContainsMethod method = this.Method.Contains method

[<Sealed>]
type ReferencedTypeMembers = class
    val private owner: ReferencedType
    val private warnings: ValidationWarningsBuilder option
    [<DefaultValue>] val mutable Method: HybridHashSet<ReferencedMethod>

    new (owner, warnings) = { owner = owner; warnings = warnings }

    member this.MethodCount = this.Method.Count

    member this.ReferenceMethod(method: ReferencedMethod) =
        match this.owner, method with
        | (:? TypeReference<TypeKinds.SealedClass>), :? MethodReference<MethodKinds.Static>
        | (:? TypeReference<TypeKinds.SealedClass>), :? MethodReference<MethodKinds.ObjectConstructor>
        | (:? TypeReference<TypeKinds.StaticClass>), :? MethodReference<MethodKinds.Static> ->
            if this.Method.Add method
            then ValidationResult.Ok(MethodCall.Referenced(this.owner, method))
            else noImpl "error for duplicate method"
        | _ -> noImpl "what referenced type and method"

    member this.ContainsMethod method = this.Method.Contains method
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
        localVarSource: Signatures.LocalVarSig -> TableIndex<StandaloneSigRow>,
        methodTokenSource: MethodTokenSource,
        body: DefinedMethodBody
    )
    =
    interface IMethodBodySource with
        member _.Create builder =
            { InitLocals = body.InitLocals
              MaxStack = body.WriteInstructions(&builder, methodTokenSource)
              LocalVariables = localVarSource body.LocalTypes }

type ModuleBuilderSerializer
    (
        moduleBuilderObj: obj,
        name: Identifier,
        mvid,
        attributes: Dictionary<_, _>,
        userStringStream,
        assembly: AssemblyDefinition option,
        entryPointToken: EntryPoint,
        assemblyReferences: HashSet<AssemblyReference>,
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
            CliHeader.defaultFields,
            CliMetadataRoot.defaultFields,
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

    let assemblyReferenceLookup = Dictionary<AssemblyReference, TableIndex<AssemblyRefRow>> assemblyReferences.Count
    let referencedTypeLookup = Dictionary<ReferencedType, _> referencedTypes.Count
    let definedTypeLookup = Dictionary<DefinedType, _> definedTypes.Count

    let methodDefSignatures = Dictionary<Signatures.MethodDefSig, MethodDefSigOffset>(definedTypeLookup.Count * 16)
    let methodRefSignatures = Dictionary<Signatures.MethodRefSig, MemberRefSigOffset>(referencedTypeLookup.Count * 32) // TODO: Helper function for getting and adding signatures.
    //let localVarSignatures = Dictionary<Signatures.LocalVarSig, TableIndex<StandaloneSigRow>>(methodDefSignatures.Count)

    let mutable methodDefParams = TableIndex<ParamRow>.One

    // TODO: Check that EventList and PropertyList actually start at 1 (they probably do)
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
        | TypeDefOrRefOrSpec.Spec tspec -> invalidOp "TODO: Get type specs"

    let localVarSource (signature: LocalVarSig<_, _>) =
        if signature.IsDefaultOrEmpty
        then Unchecked.defaultof<_>
        else
            // TODO: Consider caching local variable signatures in localVarSignatures dictionary.
            Blob.mapLocalVarSig namedTypeMapping modifierTypeMapping signature
            |> failwith "TODO: write locals to blob stream"

    let methodTokenSource = { MethodCalls = ImmutableArray.CreateBuilder(definedTypeLookup.Count * 64) }

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

            let members' = Dictionary<obj, TableIndex<MemberRefRow>>((*members.FieldCount +*) members.MethodCount)

            for method in members.Method do
                let signature = method.Signature
                members'.[method] <-
                    builder.Tables.MemberRef.Add
                        { Class = MemberRefParent.TypeRef i
                          Name = builder.Strings.Add method.Name
                          Signature =
                            match methodRefSignatures.TryGetValue signature with
                            | true, offset -> offset
                            | false, _ ->
                                let signature' = Blob.mapMethodRefSig namedTypeMapping modifierTypeMapping &signature
                                let offset = builder.Blob.Add &signature'
                                methodRefSignatures.[signature] <- offset
                                offset }

            referencedTypeLookup.[tref] <- { TypeRef = i; Members = members' }
            i

    member private this.SerializeDefinedType(tdef: DefinedType, members: DefinedTypeMembers) =
        match definedTypeLookup.TryGetValue tdef with
        | true, { TypeDef = i } -> i
        | false, _ ->
            let extends =
                match tdef.Extends with
                | ClassExtends.Null -> Unchecked.defaultof<TypeDefOrRef>
                | ClassExtends.ConcreteDef(AsDefinedType tdef)
                | ClassExtends.AbstractDef(AsDefinedType tdef) -> TypeDefOrRef.Def(this.SerializeDefinedType(tdef, members))
                | ClassExtends.ConcreteRef(AsReferencedType tref)
                | ClassExtends.AbstractRef(AsReferencedType tref) ->
                    TypeDefOrRef.Ref(this.SerializeReferencedType(tref, referencedTypes.[tref]))
                | ClassExtends.Spec tspec -> failwith "TODO: Implement writing of TypeSpecs"

            // TODO: Make a HybridHashMap helper collection type.
            let methods = Dictionary<DefinedMethod, TableIndex<MethodDefRow>> members.MethodCount

            for method in members.Method do
                for i = 0 to method.Parameters.Length - 1 do
                    let param = &method.Parameters.ItemRef i
                    let i' =
                        builder.Tables.Param.Add
                            { Flags = Parameter.flags &param
                              Name = builder.Strings.Add param.ParamName
                              Sequence = Checked.uint16(i + 1) }
                    if i = 0 then methodDefParams <- i'

                let signature = method.Signature
                let i =
                    { Rva =
                        match members.MethodBodyLookup.TryGetValue method with
                        | true, body' ->
                            let writer = DefinedMethodBodyWriter(localVarSource, methodTokenSource, body')
                            builder.MethodBodies.Add &writer
                        | false, _ -> MethodBodyLocation 0u
                      ImplFlags = method.ImplFlags
                      Flags = method.Flags
                      Name = builder.Strings.Add method.Name
                      Signature =
                        match methodDefSignatures.TryGetValue signature with
                        | true, offset -> offset
                        | false, _ ->
                            let signature' = Blob.mapMethodDefSig namedTypeMapping modifierTypeMapping &signature
                            let offset = builder.Blob.Add &signature'
                            methodDefSignatures.[signature] <- offset
                            offset
                      ParamList = methodDefParams }
                    |> builder.Tables.MethodDef.Add
                if methods.Count = 0 then indices.MethodList <- i
                methods.[method] <- i

            let i =
                builder.Tables.TypeDef.Add
                    { Flags = tdef.Flags
                      TypeName = builder.Strings.Add tdef.TypeName
                      TypeNamespace = builder.Strings.Add tdef.TypeNamespace
                      Extends = extends
                      FieldList = indices.FieldList
                      MethodList = indices.MethodList }
            
            definedTypeLookup.[tdef] <-
                { TypeDef = i
                  //Fields = fields
                  Methods = methods }
            i

    member private _.SerializeCustomAttributes() =
        /// The custom attribute table is required to be sorted.
        let customAttributeLookup = SortedList<HasCustomAttribute, _> attributes.Count

        for KeyValue(parent, attrs) in attributes do
            let key =
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
            customAttributeLookup.[key] <- attrs

        for KeyValue(parent, attrs) in customAttributeLookup do
            for (attribute: ConstructedCustomAttribute) in attrs do
                { Parent = parent
                  Type =
                    match attribute.Constructor with
                    | CustomAttributeCtor.Ref(tref, ctor) ->
                        CustomAttributeType.MemberRef referencedTypeLookup.[tref].Members.[ctor]
                    | CustomAttributeCtor.Def(tdef, ctor) ->
                        CustomAttributeType.MethodDef definedTypeLookup.[tdef].Methods.[ctor]
                  Value = builder.Blob.Add &attribute.Signature }
                |> builder.Tables.CustomAttribute.Add
                |> ignore

        builder.Tables.Sorted <- builder.Tables.Sorted ||| ValidTableFlags.CustomAttribute

    member private _.PatchMethodCalls() =
        let methodCallList = methodTokenSource.MethodCalls
        for i = 0 to methodCallList.Count - 1 do
            let call = &methodCallList.ItemRef i
            let mutable writer =
                { branchTargetList = Unchecked.defaultof<_>
                  estimatedMaxStack = 0us
                  instructions = call.InstructionWriter }
            let token =
                match call.MethodCall with
                | MethodCall.Defined(tdef, mdef) -> MethodMetadataToken.Def definedTypeLookup.[tdef].Methods.[mdef]
                | MethodCall.Referenced(tref, mref) -> MethodMetadataToken.Ref referencedTypeLookup.[tref].Members.[mref]
            Unsafe.writeCallInstruction &writer call.CallOpcode token (not call.MethodCall.Method.ReturnType.IsVoid)

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

        this.SerializeCustomAttributes()

        this.PatchMethodCalls()

        builder

[<Sealed>]
type ModuleBuilder
    (
        name,
        ?mvid,
        ?assembly: AssemblyDefinition,
        ?warnings,
        ?typeDefCapacity,
        ?typeRefCapacity,
        ?assemblyRefCapacity
    )
    as builder
    =
    let assemblyRefs = HashSet<AssemblyReference>(defaultArg assemblyRefCapacity 8)
    let definedTypes = Dictionary<DefinedType, DefinedTypeMembers>(defaultArg typeDefCapacity 16)
    let referencedTypes = Dictionary<ReferencedType, ReferencedTypeMembers>(defaultArg typeRefCapacity 32)
    let attributes = Dictionary<obj, _> 8 // TODO: Get fixed arguments!
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

    member this.DefineType t =
        match definedTypes.TryGetValue t with
        | true, existing -> ValidationResult<_>.Error(noImpl "TODO: error for duplicate type def")
        | false, _ ->
            // TODO: Check that extends and nestedclass are already contained in the Module.
            let members = DefinedTypeMembers(t, warnings, entryPointToken)
            definedTypes.[t] <- members
            Ok(struct(members, this.CreateAttributeList t))

    member _.ReferenceType t =
        match referencedTypes.TryGetValue t with
        | true, existing -> ValidationResult<_>.Error(noImpl "TODO: error for duplicate type ref")
        | false, _ ->
            // TODO: Check that the resolution scope is already accounted for.
            let members = ReferencedTypeMembers(t, warnings)
            referencedTypes.[t] <- members
            Ok members

    member _.ReferenceAssembly assem =
        if not (assemblyRefs.Add assem) then
            match warnings with
            | Some warnings' -> warnings'.Add(failwith "TODO: Warning for duplicate assembly reference")
            | None -> ()

    member internal this.Serialize() =
        let serializer =
            ModuleBuilderSerializer (
                this,
                name,
                this.Mvid,
                attributes,
                this.UserStrings,
                assembly,
                this.EntryPoint,
                assemblyRefs,
                definedTypes,
                this.GlobalMembers,
                referencedTypes
            )
        serializer.Serialize()
