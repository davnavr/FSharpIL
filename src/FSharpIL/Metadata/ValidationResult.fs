namespace FSharpIL.Metadata

open System.Collections.Immutable

/// <summary>
/// Represents a violation of a Common Language Specification rule (I.7).
/// </summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ClsViolationMessage =
    { Number: uint8
      Message: string }

    override this.ToString() = sprintf "Rule %i: %s" this.Number this.Message

[<AbstractClass>]
type ClsViolation internal (message: ClsViolationMessage) =
    member _.Message = message

[<AbstractClass>]
type ValidationWarning internal () = class end

[<AbstractClass>]
type ValidationError internal () = class end

/// II.22.1
type ValidationResult<'Result> =
    { ClsViolations: ImmutableArray<ClsViolation>
      Warnings: ImmutableArray<ValidationWarning>
      Result: Result<'Result, ValidationError> }

[<AutoOpen>]
module ValidationResultPatterns =
    let (|ValidationSuccess|ValidationWarning|ValidationError|) value =
        match value with
        | { Result = Ok result } when value.Warnings.IsEmpty -> ValidationSuccess(result, value.ClsViolations)
        | { Result = Ok result } -> ValidationWarning(result, value.ClsViolations, value.Warnings)
        | { Result = Error error } -> ValidationError error

    /// Extracts the result only if there are no warnings or CLS rule violations.
    let (|StrictSuccess|StrictWarning|StrictError|) =
        function
        | ValidationSuccess (result, cls) when cls.Length < 1 -> StrictSuccess result
        | ValidationSuccess (_, cls) -> StrictWarning(cls, ImmutableArray.Empty)
        | ValidationWarning (_, cls, warning) -> StrictWarning(cls, warning)
        | ValidationError err -> StrictError err

[<RequireQualifiedAccess>]
module ValidationResult =
    let inline success value cls =
        { ClsViolations = cls
          Warnings = ImmutableArray.Empty
          Result = Ok value }

    let inline warning value cls warnings =
        { ClsViolations = cls
          Warnings = warnings
          Result = Ok value }

    let inline error err cls warnings =
        { ClsViolations = cls
          Warnings = warnings
          Result = Error err }

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

    let ofOption none value =
        match value with
        | Some value' -> success value' ImmutableArray.Empty
        | None -> error none ImmutableArray.Empty ImmutableArray.Empty

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
