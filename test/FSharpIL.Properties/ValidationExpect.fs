﻿[<RequireQualifiedAccess>]
module FSharpIL.Metadata.ValidationExpect

open Expecto

let wantError (x: ValidationResult<_>) msg =
    match x with
    | ValidationSuccess(result, checks) ->
        failtestf "%s. Expected ValidationError, was ValidationSuccess(%A, %A)" msg result checks
    | ValidationWarning(result, checks, warnings) ->
        failtestf "%s. Expected ValidationError, was ValidationWarning(%A, %A, %A)" msg result checks warnings
    | ValidationError err -> err

let isError x msg = wantError x msg |> ignore

let isSpecificError (actual: ValidationResult<_>) (expected: ValidationError) msg =
    let result = wantError actual msg
    Expect.equal expected result msg
