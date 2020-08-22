module FSharpIL.Test

open Fuchu

[<EntryPoint>]
let main args =
    let tests =
        [
            ReadPETests.tests
        ]
        |> testList "tests"
    defaultMain tests args
