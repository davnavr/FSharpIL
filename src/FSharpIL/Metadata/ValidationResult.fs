namespace FSharpIL.Metadata

open System.Collections.Generic

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

type ValidationWarningsBuilder = ICollection<IValidationWarning>

/// A read-only collection containing the warnings produced during validation of metadata.
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ValidationWarningsCollection internal (?warnings: ValidationWarningsBuilder) =
    member _.Count =
        match warnings with
        | Some warnings' -> warnings'.Count
        | None -> 0

    member _.Contains warning =
        match warnings with
        | Some warnings' -> warnings'.Contains warning
        | None -> false

    member _.GetEnumerator() =
        match warnings with
        | Some warnings' -> warnings'.GetEnumerator()
        | None -> Seq.empty.GetEnumerator()

    interface IReadOnlyCollection<IValidationWarning> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator()
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator

[<Sealed>]
type ValidationResultBuilder internal () =
    member inline _.Bind(result: ValidationResult<_>, body: _ -> ValidationResult<_>) =
        match result with
        | Ok value -> body value
        | Error err -> Error err

    member inline _.Bind(result: IValidationError option, body: unit -> ValidationResult<_>) =
        match result with
        | None -> body()
        | Some err -> Error err

    member inline _.Combine(x: ValidationResult<unit>, y: ValidationResult<_>) =
        match x with
        | Ok() -> y
        | Error err -> Error err

    member inline _.Delay(f: _ -> ValidationResult<_>) = f()

    member inline _.Return value: ValidationResult<_> = Ok value

    member inline _.ReturnFrom(result: ValidationResult<_>) = result

    member inline _.Zero() = ValidationResult.Ok()

[<Sealed>]
type internal ValidationErrorBuilder internal () =
    member inline _.Bind(result: IValidationError option, body) =
        match result with
        | None -> body()
        | err -> err

    member inline _.Combine(x: IValidationError option, y) =
        match x with
        | None -> y
        | err -> err

    member inline _.Delay(f: _ -> IValidationError option) = f()

    member inline _.ReturnFrom(result: IValidationError option) = result

    member inline _.Zero() = Option<IValidationError>.None

[<AutoOpen>]
module ValidationResultBuilders =
    let validated = ValidationResultBuilder()
    let internal canfail = ValidationErrorBuilder()
