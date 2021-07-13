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
    val LocalTypes: Signatures.LocalVarSig
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

    member this.AddField(field: DefinedField) =
        match this.owner, field with
        | (:? TypeDefinition<TypeKinds.SealedClass>), (:? FieldDefinition<FieldKinds.Instance>) ->
            if this.Field.Add field then
                ValidationResult.Ok(FieldArg.Defined(this.owner, field))
            else noImpl "error for duplicate field"
        | _ -> noImpl (sprintf "TODO: Cannot add field %s to type %s" (field.GetType().Name) (this.owner.GetType().Name))

    // TODO: Check that the method does not use more type parameters defiend in the owner type than there actually are.
    member this.AddMethod(method: DefinedMethod, body: DefinedMethodBody voption) =
        match this.owner, method, body with
        | (:? TypeDefinition<TypeKinds.StaticClass> | :? ModuleType), (:? MethodDefinition<MethodKinds.Static>), ValueSome _
        | (:? TypeDefinition<TypeKinds.SealedClass>), (:? MethodDefinition<MethodKinds.ObjectConstructor>), ValueSome _ ->
            if this.Method.Add method then
                match body with
                | ValueSome body' -> this.MethodBodyLookup.[method] <- body'
                | ValueNone -> ()

                ValidationResult.Ok(MethodCallTarget.Defined(this.owner, method))
            else noImpl "error for duplicate method"
        | _ ->
            noImpl (sprintf "TODO: Cannot add method %s to type %s" (method.GetType().FullName) (this.owner.GetType().FullName))

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
    [<DefaultValue>] val mutable Field: HybridHashSet<ReferencedField>
    [<DefaultValue>] val mutable Method: HybridHashSet<ReferencedMethod>

    new (owner, warnings) = { owner = owner; warnings = warnings }

    member this.FieldCount = this.Field.Count
    member this.MethodCount = this.Method.Count

    member this.ReferenceMethod(method: ReferencedMethod) =
        match this.owner, method with
        | (:? TypeReference<TypeKinds.ConcreteClass> | :? TypeReference<TypeKinds.SealedClass>), :? MethodReference<MethodKinds.ObjectConstructor>
        | (:? TypeReference<TypeKinds.SealedClass> | :? TypeReference<TypeKinds.StaticClass>), :? MethodReference<MethodKinds.Static> ->
            if this.Method.Add method
            then ValidationResult.Ok(MethodCallTarget.Referenced(this.owner, method))
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
        localVarSource: Signatures.LocalVarSig -> TableIndex<StandaloneSigRow>,
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
        moduleBuilderObj: obj,
        name: Identifier,
        mvid,
        attributes: Dictionary<_, _>,
        gparams: Dictionary<_, _>,
        userStringStream,
        assembly: AssemblyDefinition option,
        entryPointToken: EntryPoint,
        assemblyReferences: HashSet<AssemblyReference>,
        definedTypes: Dictionary<DefinedType, DefinedTypeMembers>,
        moduleGlobalMembers: DefinedTypeMembers,
        referencedTypes: Dictionary<ReferencedType, ReferencedTypeMembers>,
        typeSpecSet: HashSet<TypeSpec>
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
    let typeSpecLookup = Dictionary<TypeSpec, TableIndex<TypeSpecRow>> typeSpecSet.Count

    let fieldSignatures = Dictionary<_, FieldSigOffset>((definedTypeLookup.Count + referencedTypes.Count) * 8)
    let methodDefSignatures = Dictionary<Signatures.MethodDefSig, MethodDefSigOffset>(definedTypeLookup.Count * 16)
    let methodRefSignatures = Dictionary<Signatures.MethodRefSig, MemberRefSigOffset>(referencedTypes.Count * 32) // TODO: Helper function for getting and adding signatures.
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

    let localVarSource (signature: LocalVarSig<_, _>) =
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
                let signature = &field.Signature
                members'.[field] <-
                    builder.Tables.MemberRef.Add
                        { Class = MemberRefParent.TypeRef i
                          Name = builder.Strings.Add field.Name
                          Signature =
                            match fieldSignatures.TryGetValue signature with
                            | true, { FieldSig = existing } -> { MemberRefSig = existing }
                            | false, _ ->
                                let signature' = Blob.mapFieldSig namedTypeMapping modifierTypeMapping &signature
                                let offset = builder.Blob.Add &signature'
                                fieldSignatures.[signature] <- offset
                                { MemberRefSig = offset.FieldSig } }

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
                | ClassExtends.Spec tspec -> TypeDefOrRef.Spec(this.SerializeTypeSpec tspec)

            // TODO: Make a HybridHashMap helper collection type.
            let fields = Dictionary<DefinedField, TableIndex<FieldRow>> members.FieldCount
            let methods = Dictionary<DefinedMethod, TableIndex<MethodDefRow>> members.MethodCount

            for field in members.Field do
                let signature = &field.Signature
                let i' =
                    builder.Tables.Field.Add
                        { Flags = field.Flags
                          Name = builder.Strings.Add field.Name
                          Signature =
                            match fieldSignatures.TryGetValue signature with
                            | true, existing -> existing
                            | false, _ ->
                                let signature' = Blob.mapFieldSig namedTypeMapping modifierTypeMapping &signature
                                let offset = builder.Blob.Add &signature'
                                fieldSignatures.[signature] <- offset
                                offset }
                if fields.Count = 0 then indices.FieldList <- i'
                fields.[field] <- i'

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

                let signature = method.Signature
                let i =
                    { Rva =
                        match members.MethodBodyLookup.TryGetValue method with
                        | true, body' ->
                            let writer =
                                DefinedMethodBodyWriter (
                                    localVarSource,
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
            
            // TODO: Write nested class information.

            definedTypeLookup.[tdef] <-
                { TypeDef = i
                  Fields = fields
                  Methods = methods }
            i

    member private _.SerializeTypeSpec(tspec: TypeSpec) =
        match typeSpecLookup.TryGetValue tspec with
        | true, existing -> existing
        | false, _ ->
            let i =
                builder.Tables.TypeSpec.Add
                    { TypeSpec = builder.Blob.Add(Blob.mapType namedTypeMapping modifierTypeMapping tspec) }
            typeSpecLookup.[tspec] <- i
            i

    member private _.SerializeCustomAttributes() =
        if attributes.Count > 0 then
            let lookup = SortedList<HasCustomAttribute, _> attributes.Count

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

            for KeyValue(parent, attrs: ImmutableArray<ConstructedCustomAttribute>.Builder) in lookup do
                for i = 0 to attrs.Count - 1 do
                    let attribute = &attrs.ItemRef i
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

    member private _.SerializeGenericParams() =
        if gparams.Count > 0 then
            let genericParamLookup = SortedList<TypeOrMethodDef, _> gparams.Count

            for KeyValue(parent: obj, parameters) in gparams do
                let index =
                    match parent with
                    | :? DefinedType as tdef -> TypeOrMethodDef.Type definedTypeLookup.[tdef].TypeDef
                    | _ ->
                        parent.GetType().Name
                        |> sprintf "Unsupported generic parameter parent %s, expected either a defined type or defined method"
                        |> invalidOp
                genericParamLookup.[index] <- parameters

            let genericParamConstraints = SortedList<TableIndex<GenericParamRow>, _>(genericParamLookup.Capacity * 4)

            for KeyValue(owner, GenericParamList parameters) in genericParamLookup do
                for i = 0 to parameters.Length - 1 do
                    let gparam = parameters.[i]
                    let parami =
                        let mutable flags =
                            match gparam.SpecialConstraint with
                            | NoSpecialConstriant -> GenericParamFlags.None
                            | ReferenceTypeConstraint -> GenericParamFlags.ReferenceTypeConstraint
                            | NonNullableValueTypeConstraint -> GenericParamFlags.NotNullableValueTypeConstraint

                        if gparam.RequiresDefaultConstructor then
                            flags <- flags ||| GenericParamFlags.DefaultConstructorConstraint

                        // TODO: Add variance information to generic param rows.

                        builder.Tables.GenericParam.Add
                            { Number = Checked.uint16 i
                              Flags = flags
                              Owner = owner
                              Name = builder.Strings.Add gparam.Name }

                    genericParamConstraints.[parami] <- gparam.Constraints

            for KeyValue(owner, constraints) in genericParamConstraints do
                for constr in constraints do
                    { Owner = owner
                      Constraint =
                        match constr with
                        | TypeDefOrRefOrSpec.Def tdef -> TypeDefOrRef.Def definedTypeLookup.[tdef].TypeDef
                        | TypeDefOrRefOrSpec.Ref tref -> TypeDefOrRef.Ref referencedTypeLookup.[tref].TypeRef
                        | TypeDefOrRefOrSpec.Spec tspec -> noImpl "TODO: Use lookup for typespec for generic param constraints" }
                    |> builder.Tables.GenericParamConstraint.Add
                    |> ignore

            builder.Tables.Sorted <- builder.Tables.Sorted ||| ValidTableFlags.GenericParam
            if genericParamConstraints.Count > 0 then
                builder.Tables.Sorted <- builder.Tables.Sorted ||| ValidTableFlags.GenericParamConstraint

    member private _.PatchMethodCalls() =
        let methodCallList = methodTokenSource.MethodCalls
        for i = 0 to methodCallList.Count - 1 do
            let call = &methodCallList.ItemRef i
            let mutable writer = call.Instructions
            let token =
                match call.Target with
                | MethodCallTarget.Defined(tdef, mdef) -> MethodMetadataToken.Def definedTypeLookup.[tdef].Methods.[mdef]
                | MethodCallTarget.Referenced(tref, mref) -> MethodMetadataToken.Ref referencedTypeLookup.[tref].Members.[mref]
            Unsafe.writeCallInstruction &writer call.Opcode token false // MaxStack will not be updated

    member private _.PatchFieldInstructions() =
        let fieldInstructionList = fieldTokenSource.FieldInstructions
        for i = 0 to fieldInstructionList.Count - 1 do
            let instr = &fieldInstructionList.ItemRef i
            let mutable writer = instr.Instructions
            let token =
                match instr.Target with
                | FieldArg.Defined(tdef, fdef) -> FieldMetadataToken.Def definedTypeLookup.[tdef].Fields.[fdef]
                | FieldArg.Referenced(tref, fref) -> FieldMetadataToken.Ref referencedTypeLookup.[tref].Members.[fref]
            Unsafe.writeFieldInstruction &writer instr.Opcode token false // MaxStack will not be updated

    member private _.PatchTypeInstructions() =
        let typeInstructionList = typeTokenSource.TypeInstructions
        for i = 0 to typeInstructionList.Count - 1 do
            let instr = &typeInstructionList.ItemRef i
            let mutable writer = instr.Instructions
            let token =
                match instr.Target with
                | TypeDefOrRefOrSpec.Def tdef -> TypeMetadataToken.Def definedTypeLookup.[tdef].TypeDef
                | TypeDefOrRefOrSpec.Ref tref -> TypeMetadataToken.Ref referencedTypeLookup.[tref].TypeRef
                | TypeDefOrRefOrSpec.Spec tspec -> TypeMetadataToken.Spec typeSpecLookup.[tspec]
            Unsafe.writeTypeInstruction &writer instr.Opcode token

    member this.Serialize() =
        this.SerializeAssemblyReferences()

        for KeyValue(tref, members) in referencedTypes do this.SerializeReferencedType(tref, members) |> ignore

        // Write the special <Module> type.
        this.SerializeDefinedType(ModuleType, moduleGlobalMembers) |> ignore

        for KeyValue(tdef, members) in definedTypes do this.SerializeDefinedType(tdef, members) |> ignore

        for tspec in typeSpecSet do this.SerializeTypeSpec tspec |> ignore

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
type CliModuleBuilder
    (
        name,
        ?mvid,
        ?assembly: DefinedAssembly,
        ?warnings,
        ?typeDefCapacity,
        ?typeRefCapacity,
        ?typeSpecCapacity,
        ?assemblyRefCapacity
    )
    as builder
    =
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
                referencedTypes,
                typeSpecSet
            )
        serializer.Serialize()
