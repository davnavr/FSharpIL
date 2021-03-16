module FSharpIL.FractionTests

open Expecto

open Swensen.Unquote

open CustomNumbers

[<Tests>]
let tests =
    testList "fraction" [
        testProperty "string representation contains numerator and denominator" <| fun(num, den) ->
            let str: string = (Fraction(num, den) :> obj).ToString() // TODO: Figure out how to avoid casting to object.
            test <@ str.Contains(string num) && str.Contains(string den) @>
    ]
