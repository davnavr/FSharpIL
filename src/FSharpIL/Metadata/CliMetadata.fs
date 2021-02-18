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

type BuilderResult<'T> = Result<'T, ValidationError>

type BuilderExpression<'T> = MetadataBuilderState -> BuilderResult<'T>

// TODO: Move this to another file.
[<System.Runtime.CompilerServices.IsReadOnlyAttribute>]
[<NoEquality; NoComparison>]
type TypeBuilder<'Type, 'Field, 'Method, 'GenericParam when 'Field :> IField and 'Field : equality and 'Method :> IMethod and 'Method : equality> =
    struct
        val private builder: unit -> SimpleIndex<TypeDefRow> option
        val private validate: TypeDefRow -> unit
        // TODO: Fix, forgetting to call the BuildType function will result in missing fields and methods!
        // Maybe have second index type that represent a method that might soon exist?
        // Maybe store (unit -> TypeDefRow) instances in the TypeDef table so that BuildType method is implicitly called when creating the immutable CliMetadata.
        val Fields: IndexedList<'Field>
        val Methods: IndexedList<'Method>
        val GenericParameters: IndexedList<GenericParam<'GenericParam>>

        internal new (validate, flags, typeName, typeNamespace, extends, parent, state: MetadataBuilderState) =
            let fields = IndexedList.empty<'Field> state.Owner
            let methods = IndexedList.empty<'Method> state.Owner
            let genericParams = IndexedList.empty<GenericParam<'GenericParam>> state.Owner
            let builder() =
                TypeDefRow (
                    flags,
                    typeName,
                    typeNamespace,
                    extends,
                    invalidOp "bad", // TODO: Map fields and methods
                    invalidOp "bad",
                    parent,
                    TypeBuilder<'Type, 'Field, 'Method, 'GenericParam>.GetGenericParameters genericParams
                )
                |> state.TypeDef.GetIndex
            { builder = builder
              validate = validate
              Fields = fields
              Methods = methods
              GenericParameters = genericParams }

        member this.BuildType() =
            match this.builder() with // TODO: Figure out if usage of voption will result in excess copying when used in computation expression.
            | Some index ->
                this.validate index.Value
                ValueSome index
            | None -> ValueNone

        static member GetGenericParameters(list: IndexedList<GenericParam<'GenericParam>>) =
            let builder = ImmutableArray.CreateBuilder list.Count
            for genericParam in list do
                { Flags = ValidFlags<unit, _> genericParam.Flags.Value
                  Name = genericParam.Name
                  Constaints = genericParam.Constaints }
                |> builder.Add
            builder.ToImmutable()
    end

// TODO: IMPORTANT, make builder CE return a (MetadataBuilderState -> 'T) instead of a BuilderExpression<_>

[<Sealed>]
type CliMetadataBuilder internal () =
    member inline _.Bind(expr: BuilderExpression<'T>, body: 'T -> BuilderExpression<_>): BuilderExpression<_> =
        fun state ->
            expr state |> Result.bind (fun result -> body result state)

    member inline _.Bind(expr: unit -> BuilderResult<'T>, body: 'T -> BuilderExpression<_>): BuilderExpression<_> =
        fun state ->
            expr() |> Result.bind (fun result -> body result state)

    member inline _.Bind(expr: _ -> unit, body: _ -> BuilderExpression<_>): BuilderExpression<_> =
        fun state -> expr state; body () state

    member inline _.BindCommon(expr: MetadataBuilderState -> 'T, body: _ -> BuilderExpression<_>): BuilderExpression<_> =
        fun state ->
            let result = expr state in body result state

    member inline this.Bind(expr: _ -> SimpleIndex<_>, body) = this.BindCommon(expr, body)
    member inline this.Bind(expr: _ -> TaggedIndex<_, _>, body) = this.BindCommon(expr, body)
    member inline this.Bind(expr: _ -> TypeBuilder<_, _, _, _>, body) = this.BindCommon(expr, body)

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

    /// Sets the entrypoint of the assembly.
    let setEntrypoint (main: SimpleIndex<MethodDef>) (state: MetadataBuilderState) =
        state.Owner.EnsureEqual main.Owner // TODO: Create function to check that owner is equal, and that value has its CheckOwner method called.
        state.Owner.CheckOwner main.Value
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
        TypeBuilder<'Type, 'Field, 'Method, 'GenericParam> (
            validate,
            flags,
            typeName,
            typeNamespace,
            extends,
            parent,
            state
        )

    //// TODO: Make fieldList and methodList generic.
    //let private buildTypeDef<'Type, 'GenericParamFlag> validate flags typeName typeNamespace extends parent (state: MetadataBuilderState) =
    //    let fields = IndexedList.empty<_> state.Owner
    //    let methods = IndexedList.empty<_> state.Owner
    //    let generics = IndexedList.empty<GenericParam<'GenericParamFlag>> state.Owner
    //    let builder() =
    //        let generics' =
    //            let builder = ImmutableArray.CreateBuilder generics.Count
    //            for genericParam in generics do
    //                { Flags = ValidFlags<unit, _> genericParam.Flags.Value
    //                  Name = genericParam.Name
    //                  Constaints = genericParam.Constaints }
    //                |> builder.Add
    //            builder.ToImmutable()

    //        let typeDef =
    //            TypeDefRow (
    //                flags,
    //                typeName,
    //                typeNamespace,
    //                extends,
    //                IndexedList.toBlock fields,
    //                IndexedList.toBlock methods,
    //                parent,
    //                generics'
    //            )
    //        state.TypeDef.GetIndex typeDef
    //        |> Option.map
    //            (fun i ->
    //                validate()
    //                TypeIndex<'Type> i |> Ok)
    //        |> Option.defaultValue (DuplicateValue typeDef |> Error) // TODO: Figure out how to handle error case.
    //    struct(fields, methods, generics, builder)

    // TODO: Enforce CLS checks and warnings.
    [<RequiresExplicitTypeArguments>]
    let private buildClassImpl<'Flags, 'Field, 'Method, 'GenericParam when 'Field :> IField and 'Field : equality and 'Method :> IMethod and 'Method : equality>
            (def: ClassDef<'Flags, 'Field, 'Method>)
            state =
        TypeBuilder<ClassDef<'Flags, 'Field, 'Method>, 'Field, 'Method, 'GenericParam> (
            ignore,
            (def.Flags.Value ||| def.Access.Flags),
            def.ClassName,
            def.TypeNamespace,
            def.Extends,
            def.Access.EnclosingClass,
            state
        )

    let buildClass (classDef: ConcreteClassDef) state: TypeBuilder<ConcreteClassDef, _, _, _> = buildClassImpl<_, _, _, CovariantGenericParamFlags> classDef state
    let buildAbstractClass (classDef: AbstractClassDef) state: TypeBuilder<AbstractClassDef, _, _, _> = buildClassImpl<_, _, _, CovariantGenericParamFlags> classDef state
    let buildSealedClass (classDef: SealedClassDef) state: TypeBuilder<SealedClassDef, _, _, _> = buildClassImpl<_, _, _, CovariantGenericParamFlags> classDef state
    let buildStaticClass (classDef: StaticClassDef) state: TypeBuilder<StaticClassDef, _, _, _> = buildClassImpl<_, _, _, CovariantGenericParamFlags> classDef state

    let private buildDerivedTypeDef extends def (typeDef: 'Type) (state: MetadataBuilderState) =
        match state.FindType extends with
        | Some extends' -> def extends' typeDef state |> Ok
        | None -> MissingType extends |> Error

    let buildDelegate typeDef: BuilderExpression<TypeBuilder<DelegateDef, _, _, _>> = // TODO: How to automatically add methods to delegates?
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

    let buildEnum typeDef: BuilderExpression<TypeBuilder<EnumDef, _, _, _>> = // TODO: Use custom enum field type
        buildDerivedTypeDef
            SystemType.Delegate
            (fun extends (def: EnumDef) ->
                buildTypeDef<EnumDef, _, _, unit> // TODO: What flags to use for generic parameters in enum?
                    ignore
                    def.Access.Flags
                    def.EnumName
                    def.TypeNamespace
                    (Extends.TypeRef extends)
                    def.Access.EnclosingClass)
            typeDef

    let buildStruct typeDef: BuilderExpression<TypeBuilder<StructDef, FieldChoice, _, _>> =
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

    // TODO: Allow static methods in interfaces, though they violate CLS rules.
    // TODO: What generic parameter to use in interfaces? Take advantage of the fact that the tag can be generic. Maybe add constraint?
    let buildInterface (typeDef: InterfaceDef) (state: MetadataBuilderState): TypeBuilder<InterfaceDef, StaticField, _, _> =
        buildTypeDef<_, _, _, _>
            (fun row ->
                if row.FieldList.Length > 0 then InterfaceContainsFields typeDef |> state.ClsViolations.Add)
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
