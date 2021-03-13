namespace FSharpIL.Metadata

open System.Collections.Immutable
open System.Runtime.CompilerServices

/// <summary>
/// Represents a violation of a Common Language Specification rule (I.7).
/// </summary>
[<IsReadOnly; Struct>]
type ClsViolationMessage =
    { Number: uint8
      Message: string }

    override this.ToString() = sprintf "Rule %i: %s" this.Number this.Message

/// <category>CLS Rules</category>
[<AbstractClass>]
type ClsViolation internal (message: ClsViolationMessage) =
    member _.Message = message

/// <summary>Base type of all <c>WARNING</c> checks (II.22.1).</summary>
/// <category>Warnings</category>
[<AbstractClass>]
type ValidationWarning internal () = class end

/// <summary>
/// Base type of all <c>ERROR</c> checks, which indicate that the generated CLI metadata is invalid (II.22.1).
/// </summary>
/// <category>Errors</category>
[<AbstractClass>]
type ValidationError internal () = class end

exception ValidationErrorException of ValidationError

/// II.22.1
[<IsReadOnly; Struct>]
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
    /// <exception cref="T:FSharpIL.Metadata.ValidationErrorException"/>
    let get value =
        match value with
        | ValidationSuccess (result, _)
        | ValidationWarning (result, _, _) -> result
        | ValidationError err -> ValidationErrorException err |> raise

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

type ClsViolationsBuilder = ImmutableArray<ClsViolation>.Builder
type WarningsBuilder = ImmutableArray<ValidationWarning>.Builder

[<Sealed>]
type ValidationResultBuilder internal () =
    member inline _.Bind(comp, func) =
        fun (cls: ClsViolationsBuilder) (warnings: WarningsBuilder) ->
            match comp with
            | Ok result -> func result cls warnings
            | Error (err: ValidationError) -> Error err

    member inline _.Bind(comp: ValidationResult<_>, func) =
        fun (cls: ClsViolationsBuilder) (warnings: WarningsBuilder) ->
            cls.AddRange comp.ClsViolations
            warnings.AddRange comp.Warnings
            match comp.Result with
            | Ok result -> func result cls warnings
            | Error err -> Error err

    member inline _.Return value (_: ClsViolationsBuilder) (_: WarningsBuilder) = Result<_, ValidationError>.Ok value

    member inline _.ReturnFrom(value: Result<_, ValidationError>) =
        fun (_: ClsViolationsBuilder) (_: WarningsBuilder) -> value

    member inline _.ReturnFrom(value: ValidationResult<_>) =
        fun (cls: ClsViolationsBuilder) (warnings: WarningsBuilder) ->
            cls.AddRange value.ClsViolations
            warnings.AddRange value.Warnings
            value.Result

    member inline _.Run expr =
        let cls = ImmutableArray.CreateBuilder<_>()
        let warnings = ImmutableArray.CreateBuilder<_>()
        let result = expr cls warnings
        { ClsViolations = cls.ToImmutable()
          Warnings = warnings.ToImmutable()
          Result = result }

    member inline _.TryFinally(expr, compensation) (cls: ClsViolationsBuilder) (warnings: WarningsBuilder) =
        try expr cls warnings: Result<_, ValidationError>
        finally compensation()

    member inline this.Zero() = this.Return()

[<AutoOpen>]
module ValidationResultBuilder = let validated = ValidationResultBuilder()
