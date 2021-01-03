namespace FSharpIL

/// <summary>
/// Represents a violation of a Common Language Specification rule (I.7).
/// </summary>
type ClsCheck =
    | PointerTypeUsage

/// II.22.1
type ValidationResult<'Result, 'Warning, 'Error> =
    | ValidationSuccess of 'Result * ClsCheck list
    | ValidationWarning of 'Result * ClsCheck list * 'Warning
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
