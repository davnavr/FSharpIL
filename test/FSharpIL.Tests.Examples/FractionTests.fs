module FSharpIL.FractionTests

open Expecto
open FsCheck

open Swensen.Unquote

open CustomNumbers

[<Tests>]
let tests =
    testList "fraction" [
        testCase "one-fourth is less than one-half" <| fun() ->
            test <@ Fraction(1, 4).CompareTo(Fraction(1, 2)) < 0 @>

        testCase "one-fourth times one-half is equal to one-eigth" <| fun() ->
            test <@ Fraction(1, 4) * Fraction(1, 2) = Fraction(1, 8) @>

        testProperty "string representation contains numerator and denominator" <| fun(num, den) ->
            let str = Fraction(num, den).ToString()
            test <@ str.Contains(string num) && str.Contains(string den) @>

        testProperty "numerators and denominators match" <| fun(num: int32, den: int32) ->
            failtest "Figure out why generated properties cause compiler errors"
            //<@
            //    let fraction = Fraction(num, den)
            //    fraction.Numerator = num && fraction.Denominator = den
            //@>
            //|> test
    ]
