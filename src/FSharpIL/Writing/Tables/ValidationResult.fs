namespace FSharpIL.Writing.Tables // TODO: Move these types to the FSharpIL.Metadata namespace.

open FSharpIL.Utilities

/// <summary>
/// Marker interface used to represent an <c>ERROR</c> check, which indicates that the generated CLI metadata is invalid
/// (II.22.1).
/// </summary>
/// <category>Errors</category>
type IValidationError = interface end

/// <summary>
/// Error used when an invalid combination of flags is used in a metadata table row.
/// </summary>
/// <category>Errors</category>
[<Sealed>]
type InvalidFlagsCombination<'Enum when 'Enum :> System.Enum and 'Enum : struct> (flags: 'Enum) =
    member _.Flags = flags
    override _.ToString() = sprintf "The flags combination %A is invalid" flags
    interface IValidationError

type ValidationResult<'T> = Result<'T, IValidationError>

exception ValidationErrorException
    of IValidationError
    with override this.Message = this.Data0.ToString()

[<RequireQualifiedAccess>]
module ValidationError =
    /// <summary>Raises the specified validation error as an exception.</summary>
    /// <exception cref="T:FSharpIL.Writing.Tables.ValidationErrorException">Thrown when this function is called.</exception>
    let inline throw (error: #IValidationError) = raise(ValidationErrorException error)
    let inline message (error: #IValidationError) = error.ToString()

    let inline toOption (result: ValidationResult<_>) =
        match result with
        | Ok _ -> None
        | Error err -> Some err

[<RequireQualifiedAccess>]
module ValidationResult =
    let failure (error: #IValidationError) = ValidationResult.Error error

    let inline internal (|CheckFlags|_|) flags actual =
        if Flags.set flags actual
        then Some(InvalidFlagsCombination flags :> IValidationError)
        else None

    /// <exception cref="T:FSharpIL.Writing.Tables.ValidationErrorException">
    /// Thrown when the <paramref name="result"/> is an error.
    /// </exception>
    let get (result: ValidationResult<_>) =
        match result with
        | Ok success -> success
        | Error err -> ValidationError.throw err

    let inline toValueOption (result: ValidationResult<_>) =
        match result with
        | Ok success -> ValueSome success
        | Error _ -> ValueNone

/// <summary>Marker interface used to represent a <c>WARNING</c> check (II.22.1).</summary>
/// <category>Warnings</category>
type IValidationWarning = interface end

type ValidationWarningsBuilder = System.Collections.Generic.IList<IValidationWarning>
