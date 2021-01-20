namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

/// Represents the CLI metadata header (II.25.3.3), metadata root (II.24.2.1), metadata tables (II.24.2.6), and other metadata streams.
[<Sealed>]
type CliMetadata internal (state: MetadataBuilderState) =
    // TODO: Determine if readOnlyDict or ImmutableDictionary has faster lookup times.
    let field =
        state.TypeDef
        |> Seq.collect (fun tdef -> tdef.FieldList)
        |> Seq.mapi (fun i field -> state.CreateHandle field, i)
        |> readOnlyDict
    let method =
        state.TypeDef
        |> Seq.collect (fun tdef -> tdef.MethodList)
        |> Seq.mapi (fun i method -> state.CreateHandle method, i)
        // TODO: Use ImmutableDictionary class or readOnlyDict function?
        |> readOnlyDict

    let nestedClass = state.NestedClass |> ImmutableArray.CreateRange

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
    member _.Method = method

    member val MemberRef = state.CreateTable state.MemberRef

    member val CustomAttribute = state.CustomAttribute.ToImmutableArray()

    member val Assembly = state.Assembly
    member val AssemblyRef = state.CreateTable state.AssemblyRef

    member _.NestedClass = nestedClass

    /// Gets a bit vector that indicates which tables are present (II.24.2.6).
    member val Valid: uint64 =
        let mutable bits = 1UL
        if state.TypeRef.Count > 0 then bits <- bits ||| (1UL <<< 1)
        if state.TypeDef.Count > 0 then bits <- bits ||| (1UL <<< 2)
        // if field.Count
        if method.Count > 0 then bits <- bits ||| (1UL <<< 6)

        if state.MemberRef.Count > 0 then bits <- bits ||| (1UL <<< 0xA)

        if state.CustomAttribute.Count > 0 then bits <- bits ||| (1UL <<< 0xC)

        if state.Assembly.IsSome then bits <- bits ||| (1UL <<< 0x20)
        if state.AssemblyRef.Count > 0 then bits <- bits ||| (1UL <<< 0x23)

        if nestedClass.Length > 0 then bits <- bits ||| (1UL <<< 0x29)

        bits

[<Sealed>]
type MetadataBuilder internal (mdle) =
    member inline _.Combine(one: MetadataBuilderState -> Result<_, _>, two: MetadataBuilderState -> Result<_, ValidationError>) =
        fun state ->
            match one state with
            | Ok _ -> two state
            | Error err -> Error err
    member inline _.Bind(members: Result<MemberList<'Member, _>, 'Member>, body: _ -> MetadataBuilderState -> Result<_, ValidationError>) =
        fun state ->
            match members with
            | Ok members' -> body members' state
            | Error dup -> DuplicateValue dup |> Error
    member inline _.Bind(expr: MetadataBuilderState -> unit, body: _ -> _ -> Result<_, ValidationError>) =
        fun state -> expr state; body () state
    member inline _.Bind(expr: MetadataBuilderState -> #IHandle, body: _ -> _ -> Result<_, ValidationError>) =
        fun state ->
            let result = expr state
            body result state
    member inline _.Bind(expr: MetadataBuilderState -> Result<'T, ValidationError>, body: 'T -> MetadataBuilderState -> _) =
        fun state ->
            match expr state with
            | Ok result -> body result state
            | Error err -> Error err
    member inline _.Delay(f: unit -> MetadataBuilderState -> Result<_, ValidationError>) = fun state -> f () state
    member inline _.For(items: seq<'T>, body: 'T -> MetadataBuilderState -> _) =
        fun state ->
            for item in items do
                body item state |> ignore
    member _.Run(expr: MetadataBuilderState -> _): ValidationResult<CliMetadata> =
        let state = MetadataBuilderState mdle
        match expr state with
        | Ok _ ->
            let tables = CliMetadata state
            let cls = state.ClsViolations.ToImmutable()
            if state.Warnings.Count > 0 then
                ValidationWarning(tables, cls, state.Warnings.ToImmutable())
            else
                ValidationSuccess(tables, cls)
        | Error err -> ValidationError err
    member inline _.Yield(expr: MetadataBuilderState -> #IHandle) = fun state -> expr state |> Result<_, ValidationError>.Ok
    member inline _.Yield(expr: MetadataBuilderState -> unit) = fun state -> expr state; Result<_, ValidationError>.Ok()
    member inline _.Yield(expr: MetadataBuilderState -> Result<_, ValidationError>) = expr
    member inline _.Zero() = fun _ -> Result<_, ValidationError>.Ok()

/// <summary>
/// Contains functions and computation expressions for use within the <see cref="T:FSharpIL.Metadata.MetadataBuilder"/> computation expression.
/// </summary>
[<AutoOpen>]
module MetadataBuilder =
    /// Sets the entrypoint of the assembly.
    let entrypoint (predicate: MethodDef -> bool) (tdef: TypeHandle<_>) (state: MetadataBuilderState) =
        Seq.tryFind
            predicate
            tdef.Item.MethodList
        |> Option.iter
            (fun main -> state.EntryPoint <- state.CreateHandle main |> Some)

    /// <summary>Computation Expression used for building the methods of a <see cref="T:FSharpIL.Metadata.TypeDef"/>.</summary>
    [<GeneralizableValue>]
    let methods<'Method when 'Method :> IMethod> = MemberListBuilder<'Method, _> (fun mthd -> mthd.Definition())

    /// <summary>Computation Expression used for building the fields of a <see cref="T:FSharpIL.Metadata.TypeDef"/>.</summary>
    [<GeneralizableValue>]
    let fields<'Field when 'Field :> IField> = MemberListBuilder<'Field, _> (fun field -> field.Row())
