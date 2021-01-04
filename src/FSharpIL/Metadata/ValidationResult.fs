namespace FSharpIL.Metadata

open System.Collections.Immutable

/// II.22.1
type ValidationResult<'Result, 'ClsCheck, 'Warning, 'Error> =
    | ValidationSuccess of 'Result * IImmutableList<'ClsCheck>
    | ValidationWarning of 'Result * IImmutableList<'ClsCheck> * IImmutableList<'Warning>
    | ValidationError of 'Error

[<RequireQualifiedAccess>]
module ValidationResult =
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
