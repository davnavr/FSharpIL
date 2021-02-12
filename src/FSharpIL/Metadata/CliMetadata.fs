namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Reflection

/// Represents the CLI metadata header (II.25.3.3), metadata root (II.24.2.1), metadata tables (II.24.2.6), and other metadata streams.
[<Sealed>]
type CliMetadata (state: MetadataBuilderState) =
    // TODO: Determine if readOnlyDict or ImmutableDictionary has faster lookup times.
    let field =
        let table =
            state.TypeDef
            |> Seq.collect (fun tdef -> tdef.FieldList)
            |> Seq.toArray
        ImmutableTable(table, state.CreateHandle)
    let methodDef =
        let table =
            state.TypeDef
            |> Seq.collect (fun tdef -> tdef.MethodList)
            |> Seq.toArray
        ImmutableTable(table, state.CreateHandle)
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

        if state.Assembly.IsSome then
            bits <- bits ||| (1UL <<< 0x20)
            counts.Add 1u
        if state.AssemblyRef.Count > 0 then
            bits <- bits ||| (1UL <<< 0x23)
            uint32 state.AssemblyRef.Count |> counts.Add

        if nestedClass.Length > 0 then
            bits <- bits ||| (1UL <<< 0x29)
            uint32 nestedClass.Length |> counts.Add

        bits, counts

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

    member val Assembly = state.Assembly
    member val AssemblyRef = state.CreateTable state.AssemblyRef

    member _.NestedClass = nestedClass

    /// Gets a bit vector that indicates which tables are present (II.24.2.6).
    member _.Valid: uint64 = valid

    /// <summary>
    /// Corresponds to the <c>Rows</c> field of the <c>#~</c> stream header,
    /// which specifies "the number of rows for each present table" (II.24.2.6).
    /// </summary>
    member _.RowCounts = rowCounts

type BuilderResult<'T> = Result<'T, ValidationError>

type BuilderExpression<'T> = MetadataBuilderState -> BuilderResult<'T>

[<Sealed>]
type CliMetadataBuilder internal () =
    member inline _.Bind(expr: BuilderExpression<'T>, body: 'T -> BuilderExpression<_>): BuilderExpression<_> =
        fun state ->
            expr state |> Result.bind (fun result -> body result state)

    member inline _.Bind(expr: _ -> #IHandle, body: _ -> BuilderExpression<_>): BuilderExpression<_> =
        fun state ->
            let handle = expr state in body handle state

    member inline _.Bind(members: Result<MemberList<'Member, _>, 'Member>, body: _ -> BuilderExpression<_>): BuilderExpression<_> =
        fun state ->
            match members with
            | Ok members' -> body members' state
            | Error duplicate -> DuplicateValue duplicate |> Error

    member inline _.Bind(expr: _ -> unit, body: _ -> BuilderExpression<_>): BuilderExpression<_> =
        fun state -> expr state; body () state

    member inline _.Return result: BuilderExpression<_> = fun _ -> Ok result

    member inline this.Zero() = this.Return()

/// <summary>
/// Contains functions for use with the <see cref="T:FSharpIL.Metadata.CliMetadataBuilder"/> computation expression.
/// </summary>
module CliMetadata =
    let createState (moduleTable: ModuleTable) (expr: BuilderExpression<_>) =
        let state = MetadataBuilderState moduleTable
        state, expr state

    /// <summary>
    /// Creates CLI metadata from the results of the <see cref="T:FSharpIL.Metadata.CliMetadataBuilder"/> computation expression.
    /// </summary>
    /// <seealso cref="T:FSharpIL.Builders.metadata"/>
    let createMetadata (moduleTable: ModuleTable) (expr: BuilderExpression<_>) =
        match createState moduleTable expr with
        | state, Ok() ->
            let metadata = CliMetadata state
            let cls = state.ClsViolations.ToImmutable()
            if state.Warnings.Count > 0
            then ValidationWarning(metadata, cls, state.Warnings.ToImmutable())
            else ValidationSuccess(metadata, cls)
        | _, Error err -> ValidationError err

    /// <summary>
    /// Searches for the specified method definition and sets it as the entrypoint of the assembly.
    /// </summary>
    /// <param name="predicate">A function used to determine which method is the entrypoint.</param>
    /// <param name="definingType">The type definition containing the entrypoint of the assembly.</param>
    let selectEntrypoint (predicate: MethodDef -> bool) (definingType: TypeHandle<_>) (state: MetadataBuilderState) =
        let main =
            Seq.find
                predicate
                definingType.Item.MethodList
            |> state.CreateHandle
            |> Some
        state.EntryPoint <- main

    let private addTypeDef (def: 'Type) (typeDef: TypeDef) (state: MetadataBuilderState) =
        state.TypeDef.GetHandle typeDef |> Result.map (fun def' -> { TypeHandle = def' }: TypeHandle<'Type>)

    // TODO: Enforce CLS checks and warnings.
    let private addClassImpl ({ Flags = Flags flags } as def: ClassDef<_, _, _>) (state: MetadataBuilderState) =
        let typeDef =
            TypeDefRow (
                flags ||| def.Access.Flags,
                def.ClassName,
                def.TypeNamespace,
                def.Extends,
                def.Fields.ToImmutableArray(),
                def.Methods.ToImmutableArray(),
                def.Access.EnclosingClass
            )
        addTypeDef def typeDef state

    /// <summary>
    /// Adds a <see cref="T:FSharpIL.Metadata.TypeDef"/> representing a reference type that is not marked abstract or marked sealed.
    /// </summary>
    let addClass (classDef: ConcreteClassDef): BuilderExpression<TypeHandle<ConcreteClassDef>> = addClassImpl classDef

    /// <summary>
    /// Adds a <see cref="T:FSharpIL.Metadata.TypeDef"/> representing a reference type that is marked abstract,
    /// meaning that it contains abstract methods that must be overriden by deriving classes.
    /// </summary>
    let addAbstractClass (classDef: AbstractClassDef): BuilderExpression<TypeHandle<AbstractClassDef>> = addClassImpl classDef

    /// <summary>
    /// Adds a <see cref="T:FSharpIL.Metadata.TypeDef"/> representing a reference type that is marked sealed,
    /// meaning that it cannot be derived from by other classes.
    /// </summary>
    let addSealedClass (classDef: SealedClassDef): BuilderExpression<TypeHandle<SealedClassDef>> = addClassImpl classDef

    /// <summary>
    /// Adds a <see cref="T:FSharpIL.Metadata.TypeDef"/> representing a reference type that is marked both sealed and abstract.
    /// </summary>
    let addStaticClass (classDef: StaticClassDef): BuilderExpression<TypeHandle<StaticClassDef>> = addClassImpl classDef

    let private addDerivedType extends f typeDef (state: MetadataBuilderState) =
        match state.FindType extends with
        | Some extends' ->
            let def = f extends' typeDef
            addTypeDef typeDef def state
        | None -> MissingType extends |> Error

    let addDelegate typeDef: BuilderExpression<_> =
        addDerivedType
            SystemType.Delegate
            (fun extends ({ DelegateDef.Flags = Flags flags } as def) ->
                TypeDefRow (
                    flags ||| def.Access.Flags,
                    def.DelegateName,
                    def.TypeNamespace,
                    Extends.TypeRef extends,
                    ImmutableArray.Empty,
                    ImmutableArray.Empty, // TODO: Add delegate methods.
                    def.Access.EnclosingClass
                ))
            typeDef

    let addEnum typeDef: BuilderExpression<_> =
        addDerivedType
            SystemType.Enum
            (fun extends (def: EnumDef) ->
                TypeDefRow (
                    def.Access.Flags ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable,
                    def.EnumName,
                    def.TypeNamespace,
                    Extends.TypeRef extends,
                    ImmutableArray.Empty, // TODO: Add enum values.
                    ImmutableArray.Empty, // TODO: Add enum methods, if any.
                    def.Access.EnclosingClass
                ))
            typeDef

    /// <summary>
    /// Adds a user-defined value type, which is a sealed <see cref="T:FSharpIL.Metadata.TypeDef"/>
    /// that inherits from <see cref="T:System.ValueType"/>.
    /// </summary>
    let addStruct typeDef: BuilderExpression<_> =
        addDerivedType
            SystemType.ValueType
            (fun extends ({ StructDef.Flags = Flags flags } as def) ->
                TypeDefRow (
                    flags ||| def.Access.Flags,
                    def.StructName,
                    def.TypeNamespace,
                    Extends.TypeRef extends,
                    def.Fields.ToImmutableArray(),
                    ImmutableArray.Empty, // def.Methods.ToImmutableArray(), // TODO: Add struct methods.
                    def.Access.EnclosingClass
                ))
            typeDef

    let addInterface ({ InterfaceDef.Flags = Flags flags } as typeDef) (state: MetadataBuilderState): BuilderResult<_> =
        let intf =
            TypeDefRow (
                flags ||| typeDef.Access.Flags,
                typeDef.InterfaceName,
                typeDef.TypeNamespace,
                Extends.Null,
                typeDef.Fields.ToImmutableArray(),
                ImmutableArray.Empty, //def.Methods.ToImmutableArray(), // TODO: Add interface methods.
                typeDef.Access.EnclosingClass
            )
        // TODO: Only add violation if type is successfully added.
        if typeDef.Fields.Count > 0 then InterfaceContainsFields typeDef |> state.ClsViolations.Add
        addTypeDef typeDef intf state

    let referenceType typeRef (state: MetadataBuilderState): BuilderResult<_> = state.TypeRef.GetHandle typeRef

    let referenceMethod method (state: MetadataBuilderState): BuilderResult<_> = state.MemberRef.GetHandle method

    // TODO: Better way of adding custom attributes, have a function: CustomAttribute -> target: _ -> MetadataBuilderState -> _
    let attribute attr (state: MetadataBuilderState) = state.CustomAttribute.Add attr

    /// Sets the assembly information of the metadata, which specifies the version, name, and other information concerning the .NET assembly.
    let setAssembly assembly (state: MetadataBuilderState) = state.SetAssembly assembly

    /// Adds a reference to an assembly.
    let referenceAssembly assembly (state: MetadataBuilderState) = state.AssemblyRef.GetHandle assembly

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



        match metadata.Assembly with
        | Some assembly ->
            string assembly.Name |> action
            string assembly.Culture |> action
        | None -> ()

        for assembly in metadata.AssemblyRef.Items do
            string assembly.Name |> action
            string assembly.Culture |> action

    let inline internal iterBlobs
            methodDef
            methodRef
            customAttribute
            publicKeyOrToken
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

        ()
