﻿namespace FSharpIL.Metadata

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

    member inline _.Return value: ValidationResult<_> = Ok value

    member inline _.ReturnFrom(result: ValidationResult<_>) = result

    member inline _.Zero() = ValidationResult.Ok()

[<AutoOpen>]
module ValidationResultBuilder = let validated = ValidationResultBuilder()
