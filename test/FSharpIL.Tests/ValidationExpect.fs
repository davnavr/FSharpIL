[<RequireQualifiedAccess>]
module FSharpIL.Metadata.ValidationExpect

open Expecto

let wantError (x: ValidationResult<_, _, _, _>) msg =
    match x with
    | ValidationSuccess(result, checks) ->
        failtestf "Expected ValidationError, was ValidationSuccess(%A, %A)" result checks
    | ValidationWarning(result, checks, warnings) ->
        failtestf "Expected ValidationError, was ValidationWarning(%A, %A, %A)" result checks warnings
    | ValidationError err -> err

let isError x msg = wantError x msg |> ignore
