namespace FSharpIL.Metadata

open System.Collections.Immutable

// TODO: Make a computation expression for Result<_, _> or ValidationResult<_>
// TODO: If making a module for unsafe functions, consider using a CompilerMessageAttribute as a warning.
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



        if state.MethodSpec.Count > 0 then
            bits <- bits ||| (1UL <<< 0x2B)
            uint32 state.MethodSpec.Count |> counts.Add



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

    member val MethodSpec = state.CreateTable state.MethodSpec

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

// TODO: Consider making modules for related functions. Ex: MethodRef module to contain functions to referenceDefault, refereceGeneric, etc.
/// Contains functions for modifying the CLI metadata with CLS checks and warnings.
module CliMetadata =
    [<RequireQualifiedAccess>]
    module private SystemType =
        let Delegate = "System", Identifier "Delegate"
        let Enum = "System", Identifier "Enum"
        let ValueType = "System", Identifier "ValueType"

    //[<AbstractClass; Sealed>]
    //type Unsafe = class
    //end

    /// <summary>Sets the entrypoint of the assembly.</summary>
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    let setEntryPointToken (builder: CliMetadataBuilder) entryPoint =
        IndexOwner.checkOwner builder.Owner entryPoint
        builder.SetEntryPoint entryPoint

    /// <summary>Sets the entrypoint of the assembly to a static method defined in the assembly.</summary>
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    let setEntryPoint builder main = setEntryPointToken builder (EntryPointToken.ValidEntryPoint main)

    // TODO: Enforce common CLS checks and warnings for types.
    // TODO: Consider having functions for adding typeDef, fields, and methods in separate modules.
    // TODO: Enforce CLS checks and warnings when adding a class.
    let private addClassImpl (builder: CliMetadataBuilder) (def: ClassDef<'Flags>) =
        let row =
            TypeDefRow (
                (def.Flags.Value ||| def.Access.Flags),
                def.ClassName,
                def.TypeNamespace,
                def.Extends,
                def.Access.EnclosingClass
            )
        match builder.TypeDef.TryAdd row with
        | ValueSome index -> TypeDefIndex index |> Ok
        | ValueNone -> DuplicateTypeDefError row :> ValidationError |> Error

    let addClass builder (classDef: ConcreteClassDef): Result<TypeDefIndex<ConcreteClassDef>, _> = addClassImpl builder classDef
    let addAbstractClass builder (classDef: AbstractClassDef): Result<TypeDefIndex<AbstractClassDef>, _> = addClassImpl builder classDef
    let addSealedClass builder (classDef: SealedClassDef): Result<TypeDefIndex<SealedClassDef>, _> = addClassImpl builder classDef
    let addStaticClass builder (classDef: StaticClassDef): Result<TypeDefIndex<StaticClassDef>, _> = addClassImpl builder classDef

    let private addDerivedTypeDef (builder: CliMetadataBuilder) extends def (typeDef: 'Type) =
        match state.FindType extends with
        | Some extends' -> def extends' typeDef state |> TypeDefIndex |> Ok
        | None -> MissingTypeError extends :> ValidationError |> Error

    // let addDelegate
    // let addEnum
    // let addInterface
    // let addStruct

    let addField (builder: CliMetadataBuilder) (SimpleIndex owner: TypeDefIndex<'Type>) (FieldRow field: 'Field when 'Field :> IField<'Type>) =
        match builder.Field.Add(owner, field) with
        | ValueSome index -> FieldIndex<'Field> index |> Result<FieldIndex<_>, _>.Ok
        | ValueNone -> DuplicateFieldError field :> ValidationError |> Error

    let addMethod (builder: CliMetadataBuilder) (SimpleIndex owner: TypeDefIndex<'Type>) (MethodDef method: 'Method when 'Method :> IMethod<'Type>) =
        match builder.Method.Add(owner, method) with
        | ValueSome index -> MethodDefIndex<'Method> index |> Result<MethodDefIndex<_>, _>.Ok
        | ValueNone -> DuplicateMethodError method :> ValidationError |> Error

    // TODO: Add functions for adding global fields and global methods.

    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    let referenceType (builder: CliMetadataBuilder) typeRef =
        match builder.TypeRef.TryAdd typeRef with
        | ValueSome i -> Ok i
        | ValueNone -> DuplicateTypeRefError typeRef :> ValidationError |> Error

    // TODO: Enforce CLS checks for MemberRef.
    // NOTE: Duplicates (based on owning class, name, and signature) are allowed, but produce a warning.

    let inline referenceMethod (warnings: WarningsBuilder) (builder: CliMetadataBuilder) (row: MemberRefRow) =
        let struct(i, duplicate) = builder.MemberRef.Add row
        if duplicate then warnings.Add(DuplicateMemberRefWarning row)
        i

    /// <summary>Adds a reference to a method with the <c>DEFAULT</c> calling convention.</summary>
    let referenceDefaultMethod warnings builder method : MemberRefIndex<MethodRefDefault> =
        referenceMethod warnings builder (MethodRefDefault method)

    /// <summary>Adds a reference to a method with the <c>GENERIC</c> calling convention.</summary>
    let referenceGenericMethod warnings builder method: MemberRefIndex<MethodRefGeneric> =
        referenceMethod warnings builder (MethodRefGeneric method)

    /// <summary>Adds a reference to a method with the <c>VARARG</c> calling convention.</summary>
    let referenceVarArgMethod warnings builder method: MemberRefIndex<MethodRefVarArg> =
        referenceMethod warnings builder (MethodRefVarArg method)

    // TODO: Better way of adding custom attributes, have a function: CustomAttribute -> target: _ -> MetadataBuilderState -> _
    let attribute (builder: CliMetadataBuilder) attr = builder.CustomAttribute.Add attr

    /// Sets the assembly information of the metadata, which specifies the version, name, and other information concerning the .NET assembly.
    let setAssembly (builder: CliMetadataBuilder) assembly = builder.SetAssembly assembly

    /// Adds a reference to an assembly.
    let referenceAssembly (warnings: WarningsBuilder) (builder: CliMetadataBuilder) assembly =
        let i, duplicate = builder.AssemblyRef.Add assembly
        if duplicate then warnings.Add(DuplicateAssemblyRefWarning assembly)
        i

    let addTypeSpec (builder: CliMetadataBuilder) typeSpec =
        match builder.TypeSpec.TryAdd typeSpec with
        | ValueSome index -> Ok index
        | ValueNone -> DuplicateTypeSpecError typeSpec :> ValidationError |> Error

    let addMethodSpec (builder: CliMetadataBuilder) method (garguments: seq<_>) =
        let spec' = MethodSpecRow(method, MethodSpec garguments)
        match builder.MethodSpec.TryAdd spec' with
        | ValueSome index -> Ok index
        | ValueNone -> DuplicateMethodSpecError spec' :> ValidationError |> Error

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
            match mref with
            | MethodRefDefault { MemberName = name }
            | MethodRefGeneric { MemberName = name }
            | MethodRefVarArg { MemberName = name } ->
                string name |> action

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
