namespace FSharpIL.Writing.Tables

/// <summary>
/// Marker interface used to represent an <c>ERROR</c> check, which indicates that the generated CLI metadata is invalid
/// (II.22.1).
/// </summary>
/// <category>Errors</category>
type IValidationError = interface end

type ValidationResult<'T> = Result<'T, IValidationError>

exception ValidationErrorException
    of IValidationError
    with override this.Message = this.Data0.ToString()

[<RequireQualifiedAccess>]
module ValidationError =
    let inline throw (error: #IValidationError) = raise(ValidationErrorException error)
    let inline message (error: #IValidationError) = error.ToString()

[<RequireQualifiedAccess>]
module ValidationResult =
    /// <exception cref="T:FSharpIL.Writing.Tables.ValidationErrorException">
    /// Thrown when the <paramref name="result"/> is an error.
    /// </exception>
    let get (result: ValidationResult<_>) =
        match result with
        | Ok success -> success
        | Error err -> ValidationError.throw err

    let toValueOption (result: ValidationResult<_>) =
        match result with
        | Ok success -> ValueSome success
        | Error _ -> ValueNone

/// <summary>Marker interface used to represent a <c>WARNING</c> check (II.22.1).</summary>
/// <category>Warnings</category>
type IValidationWarning = interface end

type ValidationWarningsBuilder = System.Collections.Generic.IList<IValidationWarning>
