module FSharpIL.FactorialTests

open Expecto

open Swensen.Unquote

open Factorial

[<Tests>]
let tests =
    testList "fraction" [
        testCase "3 factorial is 6" <| fun() -> test <@ CachedFactorial.Calculate 3u = 6u @>
        // TODO: Use a property test can be used to test factorial calculation.
    ]
