module FSharpIL.FractionTests

open Expecto

open Swensen.Unquote

open CustomNumbers

[<Tests>]
let tests =
    testList "fraction" [
        testProperty "string representation contains numerator and denominator" <| fun(num, den) ->
            let str = Fraction(num, den).ToString()
            test <@ str.Contains(string num) && str.Contains(string den) @>

        testCase "one-fourth is less than one-half" <| fun() ->
            test <@ Fraction(1, 4).CompareTo(Fraction(1, 2)) < 0 @>
    ]
