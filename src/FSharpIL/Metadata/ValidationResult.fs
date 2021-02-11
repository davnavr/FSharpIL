namespace FSharpIL.Metadata

open System.Collections.Immutable

/// II.22.1
type ValidationResult<'Result> =
    | ValidationSuccess of 'Result * IImmutableList<ClsViolation>
    | ValidationWarning of 'Result * IImmutableList<ClsViolation> * IImmutableList<ValidationWarning>
    | ValidationError of ValidationError

    member this.Warnings =
        match this with
        | ValidationWarning (_, _, warnings) -> warnings
        | _ -> ImmutableList.Empty :> IImmutableList<_>

    member this.ClsChecks =
        match this with
        | ValidationSuccess (_, checks)
        | ValidationWarning (_, checks, _) -> checks
        | _ -> ImmutableList.Empty :> IImmutableList<_>

    member this.IsError =
        match this with
        | ValidationError _ -> true
        | ValidationSuccess _
        | ValidationWarning _ -> false

[<RequireQualifiedAccess>]
module ValidationResult =
    /// <summary>
    /// Retrieves the value associated with the result.
    /// </summary>
    /// <exception cref="T:System.ArgumentException">
    /// The <paramref name="value"/> is a <see cref="T:FSharpIL.Metadata.ValidationResult`1.ValidationError"/>.
    /// </exception>
    let get value =
        match value with
        | ValidationSuccess (result, _)
        | ValidationWarning (result, _, _) -> result
        | ValidationError err -> string err |> invalidArg (nameof value)

    let toOption value =
        match value with
        | ValidationSuccess (result, _)
        | ValidationWarning (result, _, _) -> Some result
        | ValidationError _ -> None

    let toResult value =
        match value with
        | ValidationSuccess (result, _)
        | ValidationWarning (result, _, _) -> Ok result
        | ValidationError err -> Error err

[<AutoOpen>]
module ValidationResultPatterns =
    /// Extracts the result only if there are no warnings or CLS rule violations.
    let (|StrictSuccess|StrictWarning|StrictError|) =
        function
        | ValidationSuccess (result, cls) when cls.Count < 1 -> Choice1Of3 result
        | ValidationSuccess (_, cls) -> Choice2Of3(cls, ImmutableArray.Empty :> IImmutableList<_>)
        | ValidationWarning (_, cls, warning) -> Choice2Of3(cls, warning)
        | ValidationError err -> Choice3Of3 err
