namespace FSharpIL.Metadata

open System.Collections.Immutable

/// II.22.1
type ValidationResult<'Result, 'ClsCheck, 'Warning, 'Error> =
    | ValidationSuccess of 'Result * IImmutableList<'ClsCheck>
    | ValidationWarning of 'Result * IImmutableList<'ClsCheck> * IImmutableList<'Warning>
    | ValidationError of 'Error

[<RequireQualifiedAccess>]
module ValidationResult =
    let get value =
        match value with
        | ValidationSuccess (result, _)
        | ValidationWarning (result, _, _) -> result
        | ValidationError err -> string err |> invalidArg "value"

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
