namespace FSharpIL.Metadata

open System.Collections.Immutable

/// II.24.2.6
[<Sealed>]
type MetadataTables internal (state: MetadataBuilderState) =
    /// A collection of warnings produced while creating the metadata.
    member val Warnings = state.Warnings.ToImmutable()
    member val ClsViolations = state.ClsViolations.ToImmutable()

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

    static member val Default = MetadataBuilderState() |> MetadataTables

[<Sealed>]
type MetadataBuilder internal () =
    member inline _.Combine(one: MetadataBuilderState -> Result<_, _>, two: MetadataBuilderState -> Result<_, ValidationError>) =
        fun state ->
            match one state with
            | Ok _ -> two state
            | Error err -> Error err
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
        let state = MetadataBuilderState()
        match expr state with
        | Ok _ ->
            let tables = MetadataTables state
            if state.Warnings.Count > 0 then
                ValidationWarning(tables, tables.ClsViolations, tables.Warnings)
            else
                ValidationSuccess(tables, tables.ClsViolations)
        | Error err -> ValidationError err
    member inline _.Yield(expr: MetadataBuilderState -> Handle<_>) = fun state -> expr state |> Result<_, ValidationError>.Ok
    member inline _.Yield(expr: MetadataBuilderState -> Result<_, ValidationError>) = expr
    member inline _.Zero() = fun _ -> Result<_, ValidationError>.Ok()

/// <summary>
/// Contains functions for use within the <see cref="T:FSharpIL.Metadata.MetadataBuilder"/> computation expression.
/// </summary>
[<AutoOpen>]
module MetadataBuilder =
    /// Sets the module information of the metadata.
    let inline mdle (mdle: ModuleTable) (state: MetadataBuilderState) = state.Module <- mdle
    /// Sets the assembly information of the metadata, which specifies the version, name, and other information concerning the .NET assembly.
    let inline assembly (assembly: Assembly) (state: MetadataBuilderState) = state.Assembly <- Some assembly
