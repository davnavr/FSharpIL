module FSharpIL.DelegatesTests

open Expecto
open FsCheck

open Swensen.Unquote

[<Tests>]
let tests =
    testList "delegates example" [
        testList "using static method" [
            testProperty "works with F# function" <| fun(arg1, arg2, output: string) ->
                let sut = MyDelegate(fun _ _ -> output)
                test <@ sut.Invoke(arg1, arg2) = output @>

            testProperty "duplicate string returns string with correct length" <| fun(NonNull str, PositiveInt times) ->
                let sut = MyDelegate(fun arg1 arg2 -> MyClass.DuplicateString(arg1, arg2))
                test <@ sut.Invoke(str, times).Length = str.Length * times @>
        ]

        testList "using instance method" [
            testProperty "works with F# function" <| fun(arg1, arg2, f) ->
                let sut = MyDelegate f
                test <@ sut.Invoke(arg1, arg2) = f arg1 arg2 @>
        ]
    ]
