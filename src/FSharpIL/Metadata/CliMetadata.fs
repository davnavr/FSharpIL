namespace FSharpIL.Metadata

open System.Collections.Immutable

/// Represents the CLI metadata header (II.25.3.3), metadata root (II.24.2.1), metadata tables (II.24.2.6), and other metadata streams.
[<Sealed>]
type CliMetadata (state: MetadataBuilderState) =
    // TODO: Determine if readOnlyDict or ImmutableDictionary has faster lookup times.
    let methodDef = state.CreateTable state.Method
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
        if state.Field.Count > 0 then
            bits <- bits ||| (1UL <<< 4)
            uint32 state.Field.Count |> counts.Add
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
        if state.TypeSpec.Count > 0 then
            bits <- bits ||| (1UL <<< 0x1B)
            uint32 state.TypeSpec.Count |> counts.Add


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
    member val Field = state.CreateTable state.Field
    member _.MethodDef = methodDef
    member _.Param = parameters

    member val MemberRef = state.CreateTable state.MemberRef

    member val CustomAttribute = state.CustomAttribute.ToImmutableArray()

    member val ModuleRef = state.CreateTable state.ModuleRef
    member val TypeSpec = state.CreateTable state.TypeSpec

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

    // TODO: Create separate immutable table type called OwnedTable that stores the counts for each owner.
    member val internal TempFieldCounts =
        state.Field.ToReadOnlyDictionary()
        |> Seq.map (fun (KeyValue (tdef, field)) -> tdef, uint32 field.Count)
        |> readOnlyDict

    member val internal TempMethodCounts =
        state.Method.ToReadOnlyDictionary()
        |> Seq.map (fun (KeyValue (tdef, method)) -> tdef, uint32 method.Count)
        |> readOnlyDict

/// Contains functions for modifying the CLI metadata.
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
        // TODO: Mention name of offending interface when it contains fields.
        inherit ClsViolation({ Number = 19uy; Message = sprintf "Interfaces should not define fields" })
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

    /// <summary>Sets the entrypoint of the assembly.</summary>
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    let setEntryPointToken entryPoint (state: MetadataBuilderState) =
        IndexOwner.checkOwner state.Owner entryPoint
        state.EntryPoint <- ValueSome entryPoint

    /// <summary>Sets the entrypoint of the assembly to a static method defined in the assembly.</summary>
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    let setEntryPoint main state = setEntryPointToken (EntryPointToken.ValidEntryPoint main) state

    // TODO: Consider having functions for adding typeDef, fields, and methods in separate modules.
    // TODO: Enforce CLS checks and warnings when adding a class.
    let private addClassImpl (def: ClassDef<'Flags>) (state: MetadataBuilderState) =
        TypeDefRow (
            (def.Flags.Value ||| def.Access.Flags),
            def.ClassName,
            def.TypeNamespace,
            def.Extends,
            def.Access.EnclosingClass
        )
        |> state.TypeDef.GetIndex
        |> Result.map TypeIndex<ClassDef<'Flags>>

    let addClass (classDef: ConcreteClassDef) state: Result<TypeIndex<ConcreteClassDef>, _> = addClassImpl classDef state
    let addAbstractClass (classDef: AbstractClassDef) state: Result<TypeIndex<AbstractClassDef>, _> = addClassImpl classDef state
    let addSealedClass (classDef: SealedClassDef) state: Result<TypeIndex<SealedClassDef>, _> = addClassImpl classDef state
    let addStaticClass (classDef: StaticClassDef) state: Result<TypeIndex<StaticClassDef>, _> = addClassImpl classDef state

    let private addDerivedTypeDef extends def (typeDef: 'Type) (state: MetadataBuilderState) =
        match state.FindType extends with
        | Some extends' -> def extends' typeDef state |> TypeIndex<'Type> |> Ok
        | None -> MissingTypeError extends :> ValidationError |> Error

    // let addDelegate
    // let addEnum
    // let addInterface
    // let addStruct

    let addField (SimpleIndex owner: TypeIndex<'Type>) (FieldRow field: 'Field when 'Field :> IField<'Type>) (state: MetadataBuilderState) =
        match state.Field.Add(owner, field) with
        | ValueSome index -> FieldIndex<'Field> index |> Result<FieldIndex<_>, _>.Ok
        | ValueNone -> DuplicateFieldError field :> ValidationError |> Error

    let addMethod (SimpleIndex owner: TypeIndex<'Type>) (MethodDef method: 'Method when 'Method :> IMethod<'Type>) (state: MetadataBuilderState) =
        match state.Method.Add(owner, method) with
        | ValueSome index -> MethodIndex<'Method> index |> Result<MethodIndex<_>, _>.Ok
        | ValueNone -> DuplicateMethodError method :> ValidationError |> Error

    // TODO: Add functions for adding global fields and global methods.

    let referenceType typeRef (state: MetadataBuilderState) = state.TypeRef.GetIndex typeRef
    let referenceMethod method (state: MetadataBuilderState): MemberRefIndex<MethodRef> = state.MemberRef.GetIndex method

    // TODO: Better way of adding custom attributes, have a function: CustomAttribute -> target: _ -> MetadataBuilderState -> _
    let attribute attr (state: MetadataBuilderState) = state.CustomAttribute.Add attr

    /// Sets the assembly information of the metadata, which specifies the version, name, and other information concerning the .NET assembly.
    let setAssembly assembly (state: MetadataBuilderState) = state.SetAssembly assembly

    /// Adds a reference to an assembly.
    let referenceAssembly assembly (state: MetadataBuilderState) = state.AssemblyRef.GetIndex assembly

    let addTypeSpec typeSpec (state: MetadataBuilderState) =
        match state.TypeSpec.GetIndex typeSpec with
        | ValueSome index -> Ok index
        | ValueNone -> DuplicateTypeSpecError typeSpec :> ValidationError |> Error

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
