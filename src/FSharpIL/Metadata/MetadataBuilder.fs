namespace FSharpIL.Metadata

open System.Collections.Immutable

/// II.24.2.6
[<Sealed>]
type MetadataTables internal (state: MetadataBuilderState) =
    member val MajorVersion = state.MajorVersion
    member val MinorVersion = state.MinorVersion
    member val Module = state.Module
    member val TypeRef = state.CreateTable state.TypeRef
    member val TypeDef = state.CreateTable state.TypeDef

    member val Assembly = state.Assembly
    member val AssemblyRef = state.CreateTable state.AssemblyRef

    member val NestedClass = state.NestedClass |> ImmutableArray.CreateRange

    /// Gets a bit vector that indicates which tables are present.
    member val Valid: uint64 =
        // TODO: Update this based on the tables.
        /// NOTE: Bit zero appears to be the right-most bit.
        0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000001UL

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
    member _.Run(expr: MetadataBuilderState -> _): ValidationResult<MetadataTables> =
        let state = MetadataBuilderState mdle
        match expr state with
        | Ok _ ->
            let tables = MetadataTables state
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
/// Contains functions for use within the <see cref="T:FSharpIL.Metadata.MetadataBuilder"/> computation expression.
/// </summary>
[<AutoOpen>]
module MetadataBuilder =
    /// Sets the assembly information of the metadata, which specifies the version, name, and other information concerning the .NET assembly.
    let inline assembly (assembly: Assembly) (state: MetadataBuilderState) = state.Assembly <- Some assembly

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
