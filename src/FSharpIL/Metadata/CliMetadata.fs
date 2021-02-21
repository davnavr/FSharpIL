namespace FSharpIL.Metadata

open System.Collections.Immutable

/// Represents the CLI metadata header (II.25.3.3), metadata root (II.24.2.1), metadata tables (II.24.2.6), and other metadata streams.
[<Sealed>]
type CliMetadata (state: MetadataBuilderState) =
    // TODO: Determine if readOnlyDict or ImmutableDictionary has faster lookup times.
    let field =
        let table =
            state.TypeDef
            |> Seq.collect (fun tdef -> tdef.FieldList)
            |> Seq.toArray
        state.CreateTable table
    let methodDef =
        let table =
            state.TypeDef
            |> Seq.collect (fun tdef -> tdef.MethodList)
            |> Seq.toArray
        state.CreateTable table
    let parameters =
        methodDef.Items
        |> Seq.collect(fun method -> Seq.indexed method.ParamList)
        |> ImmutableArray.CreateRange

    let nestedClass = state.NestedClass |> ImmutableArray.CreateRange

    let valid, rowCounts =
        // Module table is always present
        let mutable bits = 1UL
        let counts = ImmutableArray.CreateBuilder 48
        counts.Add 1u

        if state.TypeRef.Count > 0 then
            bits <- bits ||| (1UL <<< 1)
            uint32 state.TypeRef.Count |> counts.Add
        if state.TypeDef.Count > 0 then
            bits <- bits ||| (1UL <<< 2)
            uint32 state.TypeDef.Count |> counts.Add
        // if field.Count
        if methodDef.Count > 0 then
            bits <- bits ||| (1UL <<< 6)
            uint32 methodDef.Count |> counts.Add
        if not parameters.IsEmpty then
            bits <- bits ||| (1UL <<< 8)
            uint32 parameters.Length |> counts.Add



        if state.MemberRef.Count > 0 then
            bits <- bits ||| (1UL <<< 0xA)
            uint32 state.MemberRef.Count |> counts.Add



        if state.CustomAttribute.Count > 0 then
            bits <- bits ||| (1UL <<< 0xC)
            uint32 state.CustomAttribute.Count |> counts.Add



        if state.ModuleRef.Count > 0 then
            bits <- bits ||| (1UL <<< 0x1A)
            uint32 state.ModuleRef.Count |> counts.Add



        if state.Assembly.IsSome then
            bits <- bits ||| (1UL <<< 0x20)
            counts.Add 1u
        if state.AssemblyRef.Count > 0 then
            bits <- bits ||| (1UL <<< 0x23)
            uint32 state.AssemblyRef.Count |> counts.Add



        if state.File.Count > 0 then
            bits <- bits ||| (1UL <<< 0x26)
            uint32 state.File.Count |> counts.Add



        if nestedClass.Length > 0 then
            bits <- bits ||| (1UL <<< 0x29)
            uint32 nestedClass.Length |> counts.Add

        bits, counts

    member val internal Owner = state.Owner

    member val Header = state.Header
    /// <summary>Corresponds to the <c>Flags</c> field of the CLI header (II.25.3.3).</summary>
    member val HeaderFlags = state.HeaderFlags
    /// <summary>Corresponds to the <c>EntryPointToken</c> field of the CLI header (II.25.3.3).</summary>
    member val EntryPointToken = state.EntryPoint

    /// <summary>Corresponds to the <c>Version</c> field of the metadata root (II.24.2.1)</summary>
    member val MetadataVersion = state.MetadataVersion

    /// <summary>Corresponds to the <c>MajorVersion</c> field of the <c>#~</c> stream header.</summary>
    member val MajorVersion = state.MajorVersion
    /// <summary>Corresponds to the <c>MinorVersion</c> field of the <c>#~</c> stream header.</summary>
    member val MinorVersion = state.MinorVersion

    member val Module = state.Module
    member val TypeRef = state.CreateTable state.TypeRef
    member val TypeDef = state.CreateTable state.TypeDef
    member _.Field = field
    member _.MethodDef = methodDef
    member _.Param = parameters

    member val MemberRef = state.CreateTable state.MemberRef

    member val CustomAttribute = state.CustomAttribute.ToImmutableArray()

    member val ModuleRef = state.CreateTable state.ModuleRef

    member val Assembly = state.Assembly
    member val AssemblyRef = state.CreateTable state.AssemblyRef

    member val File = state.CreateTable state.File

    member _.NestedClass = nestedClass

    /// Gets a bit vector that indicates which tables are present (II.24.2.6).
    member _.Valid: uint64 = valid

    /// <summary>
    /// Corresponds to the <c>Rows</c> field of the <c>#~</c> stream header,
    /// which specifies "the number of rows for each present table" (II.24.2.6).
    /// </summary>
    member _.RowCounts = rowCounts

// TODO: Move this to another file.
// TODO: Make this a class.
[<System.Runtime.CompilerServices.IsReadOnlyAttribute>]
[<NoEquality; NoComparison>]
type TypeDefBuilder<'Type, 'Field, 'Method, 'GenericParam when 'Field :> IField and 'Field : equality and 'Method :> IMethod and 'Method : equality> =
    struct
        val private builder: unit -> Result<SimpleIndex<TypeDefRow>, ValidationError>
        val private validate: TypeDefRow -> unit
        // TODO: Fix, forgetting to call the BuildType function will result in missing fields and methods!
        val private fields: IndexedListBuilder<FieldRow>
        val private methods: IndexedListBuilder<MethodDef>
        val private genericParams: GenericParamList<'GenericParam>

        internal new (validate, flags, typeName, typeNamespace, extends, parent, state: MetadataBuilderState) =
            let fields = IndexedListBuilder state.Owner
            let methods = IndexedListBuilder state.Owner
            let genericParams = GenericParamList<'GenericParam> state.Owner
            let builder() =
                TypeDefRow (
                    flags,
                    typeName,
                    typeNamespace,
                    extends,
                    fields.ToImmutable(),
                    methods.ToImmutable(),
                    parent,
                    genericParams.ToImmutable()
                )
                |> state.TypeDef.GetIndex
            { builder = builder
              validate = validate
              fields = fields
              methods = methods
              genericParams = genericParams }

        member this.AddMethod(MethodDef method: 'Method) =
            match this.methods.Add method with
            | ValueSome i -> Ok i
            | ValueNone -> DuplicateMethodError method :> ValidationError |> Error

        // AddField
        // TODO: Figure out how to add generic parameters.

        // TODO: Create functions for adding things to a TypeBuilder.

        member this.BuildType() =
            match this.builder() with
            | Ok tdef ->
                this.validate tdef.Value
                Ok tdef
            | err -> err
    end

module CliMetadata =
    [<RequireQualifiedAccess>]
    module private SystemType =
        let Delegate = "System", Identifier "Delegate"
        let Enum = "System", Identifier "Enum"
        let ValueType = "System", Identifier "ValueType"

    /// <summary>Represents a violation of CLS rule 19, which states that "CLS-compliant interfaces shall not define...fields".</summary>
    /// <category>CLS Rules</category>
    [<Sealed>]
    type InterfaceContainsFields (intf: InterfaceDef) =
        inherit ClsViolation({ Number = 19uy; Message = sprintf "Interfaces should not define fields" }) // TODO: Mention name of offending interface
        member _.Interface = intf

    /// <summary>
    /// Error used when a system type such as <see cref="T:System.ValueType"/> or <see cref="T:System.Delegate"/> could not be found.
    /// </summary>
    /// <category>Errors</category>
    [<Sealed>]
    type MissingTypeError (ns: string, name: Identifier) =
        inherit ValidationError()
        member _.Namespace = ns
        member _.Name = name
        override _.ToString() =
            match ns with
            | "" -> string name
            | _ -> sprintf "%s.%A" ns name
            |> sprintf "Unable to find type \"%s\", perhaps a TypeDef or TypeRef is missing"

    let createState (moduleTable: ModuleTable) (expr: MetadataBuilderState -> _) =
        let state = MetadataBuilderState moduleTable
        state, expr state

    /// <summary>
    /// Creates CLI metadata from the results of the <see cref="T:FSharpIL.Metadata.CliMetadataBuilder"/> computation expression.
    /// </summary>
    /// <seealso cref="T:FSharpIL.Builders.metadata"/>
    let createMetadata (moduleTable: ModuleTable) (expr: MetadataBuilderState -> Result<unit, ValidationError>) =
        let state, result = createState moduleTable expr
        let cls = state.ClsViolations.ToImmutable()
        let warnings = state.Warnings.ToImmutable()

        match result with
        | Ok() ->
            let metadata = CliMetadata state
            if warnings.IsEmpty
            then ValidationResult.success metadata cls
            else ValidationResult.warning metadata cls warnings
        | Error err -> ValidationResult.error err cls warnings

    /// Sets the entrypoint of the assembly.
    let setEntrypoint (main: SimpleIndex<MethodDef>) (state: MetadataBuilderState) =
        IndexOwner.checkIndex state.Owner main
        state.EntryPoint <- Some main

    [<RequiresExplicitTypeArguments>]
    let private buildTypeDef<'Type, 'Field, 'Method, 'GenericParam when 'Field :> IField and 'Field : equality and 'Method :> IMethod and 'Method : equality>
            validate
            flags
            typeName
            typeNamespace
            extends
            parent
            (state: MetadataBuilderState) =
        TypeDefBuilder<'Type, 'Field, 'Method, 'GenericParam> (
            validate,
            flags,
            typeName,
            typeNamespace,
            extends,
            parent,
            state
        )

    // TODO: Enforce CLS checks and warnings.
    [<RequiresExplicitTypeArguments>]
    let private buildClassImpl<'Flags, 'Field, 'Method, 'GenericParam when 'Field :> IField and 'Field : equality and 'Method :> IMethod and 'Method : equality>
            (def: ClassDef<'Flags, 'Field, 'Method>)
            state =
        TypeDefBuilder<ClassDef<'Flags, 'Field, 'Method>, 'Field, 'Method, 'GenericParam> (
            ignore,
            (def.Flags.Value ||| def.Access.Flags),
            def.ClassName,
            def.TypeNamespace,
            def.Extends,
            def.Access.EnclosingClass,
            state
        )

    let buildClass (classDef: ConcreteClassDef) state: TypeDefBuilder<ConcreteClassDef, _, _, _> = buildClassImpl<_, _, _, CovariantGenericParamFlags> classDef state
    let buildAbstractClass (classDef: AbstractClassDef) state: TypeDefBuilder<AbstractClassDef, _, _, _> = buildClassImpl<_, _, _, CovariantGenericParamFlags> classDef state
    let buildSealedClass (classDef: SealedClassDef) state: TypeDefBuilder<SealedClassDef, _, _, _> = buildClassImpl<_, _, _, CovariantGenericParamFlags> classDef state
    let buildStaticClass (classDef: StaticClassDef) state: TypeDefBuilder<StaticClassDef, _, _, _> = buildClassImpl<_, _, _, CovariantGenericParamFlags> classDef state

    let private buildDerivedTypeDef extends def (typeDef: 'Type) (state: MetadataBuilderState) =
        match state.FindType extends with
        | Some extends' -> def extends' typeDef state |> Ok
        | None -> MissingTypeError extends :> ValidationError |> Error

    let buildDelegate typeDef state: Result<TypeDefBuilder<DelegateDef, _, _, _>, _> = // TODO: How to automatically add methods to delegates?
        buildDerivedTypeDef
            SystemType.Delegate
            (fun extends (def: DelegateDef) ->
                buildTypeDef<DelegateDef, _, _, unit> // TODO: What flags to use for generic parameters in delegates?
                    ignore
                    (def.Flags.Value ||| def.Access.Flags)
                    def.DelegateName
                    def.TypeNamespace
                    (Extends.TypeRef extends)
                    def.Access.EnclosingClass)
            typeDef
            state

    let buildEnum typeDef state: Result<TypeDefBuilder<EnumDef, _, _, _>, _> = // TODO: Use custom enum field type
        buildDerivedTypeDef
            SystemType.Enum
            (fun extends (def: EnumDef) ->
                buildTypeDef<EnumDef, _, _, unit> // TODO: What flags to use for generic parameters in enum?
                    ignore
                    def.Access.Flags
                    def.EnumName
                    def.TypeNamespace
                    (Extends.TypeRef extends)
                    def.Access.EnclosingClass)
            typeDef
            state

    let buildStruct typeDef state: Result<TypeDefBuilder<StructDef, FieldChoice, _, _>, _> =
        buildDerivedTypeDef
            SystemType.ValueType
            (fun extends (def: StructDef) ->
                buildTypeDef<StructDef, _, _, CovariantGenericParamFlags> // TODO: What flags to use for generic parameters in structs?
                    ignore
                    (def.Flags.Value ||| def.Access.Flags)
                    def.StructName
                    def.TypeNamespace
                    (Extends.TypeRef extends)
                    def.Access.EnclosingClass)
            typeDef
            state

    // TODO: Allow static methods in interfaces, though they violate CLS rules.
    // TODO: What generic parameter to use in interfaces? Take advantage of the fact that the tag can be generic. Maybe add constraint?
    let buildInterface (typeDef: InterfaceDef) (state: MetadataBuilderState): TypeDefBuilder<InterfaceDef, StaticField, _, _> =
        buildTypeDef<_, _, _, _>
            (fun row ->
                if row.FieldList.Count > 0 then InterfaceContainsFields typeDef |> state.ClsViolations.Add)
            (typeDef.Flags.Value ||| typeDef.Access.Flags)
            typeDef.InterfaceName
            typeDef.TypeNamespace
            Extends.Null
            typeDef.Access.EnclosingClass
            state

    let referenceType typeRef (state: MetadataBuilderState) = state.TypeRef.GetIndex typeRef

    let referenceMethod method (state: MetadataBuilderState): MemberRefIndex<MethodRef> = state.MemberRef.GetIndex method

    // TODO: Better way of adding custom attributes, have a function: CustomAttribute -> target: _ -> MetadataBuilderState -> _
    let attribute attr (state: MetadataBuilderState) = state.CustomAttribute.Add attr

    /// Sets the assembly information of the metadata, which specifies the version, name, and other information concerning the .NET assembly.
    let setAssembly assembly (state: MetadataBuilderState) = state.SetAssembly assembly

    /// Adds a reference to an assembly.
    let referenceAssembly assembly (state: MetadataBuilderState) = state.AssemblyRef.GetIndex assembly

    /// <summary>
    /// Applies the given function to each string in the <c>#Strings</c> heap referenced in
    /// the CLI metadata tables (II.24.2.6).
    /// </summary>
    let inline internal iterStrings action (metadata: CliMetadata) =
        string metadata.Module.Name |> action

        for tref in metadata.TypeRef.Items do
            string tref.TypeName |> action
            action tref.TypeNamespace

        for tdef in metadata.TypeDef.Items do
            string tdef.TypeName |> action
            action tdef.TypeNamespace

        for field in metadata.Field.Items do
            string field.Name |> action

        for method in metadata.MethodDef.Items do
            string method.Name |> action

        for _, param in metadata.Param do
            action param.ParamName



        for mref in metadata.MemberRef.Items do
            string mref.MemberName |> action

        // ModuleRef table not necessary, since its names will correspond to names used in the File table

        match metadata.Assembly with
        | Some assembly ->
            string assembly.Name |> action
            string assembly.Culture |> action
        | None -> ()

        for assembly in metadata.AssemblyRef.Items do
            string assembly.Name |> action
            string assembly.Culture |> action

        for { FileName = name } in metadata.File.Items do
            string name |> action

    let inline internal iterBlobs
            methodDef
            methodRef
            customAttribute
            publicKeyOrToken
            bytes
            (metadata: CliMetadata) =
        for method in metadata.MethodDef.Items do
            methodDef method.Signature

        for mref in metadata.MemberRef.Items do
            match mref with
            | MethodRef method -> methodRef method.Signature

        for { Value = signature } in metadata.CustomAttribute do
            Option.iter customAttribute signature

        for { PublicKeyOrToken = token } in metadata.AssemblyRef.Items do
            publicKeyOrToken token

        for { File.HashValue = hashValue } in metadata.File.Items do
            bytes hashValue

        ()
