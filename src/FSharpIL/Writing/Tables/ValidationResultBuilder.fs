namespace FSharpIL.Writing.Tables // TODO: Move these types to the FSharpIL.Metadata namespace.

[<Sealed>]
type ValidationResultBuilder internal () =
    member inline _.Bind(result: ValidationResult<_>, body: _ -> ValidationResult<_>) =
        match result with
        | Ok value -> body value
        | Error err -> Error err

    member inline _.Return value: ValidationResult<_> = Ok value

    member inline _.ReturnFrom(result: ValidationResult<_>) = result

[<AutoOpen>]
module ValidationResultBuilder = let validated = ValidationResultBuilder()
