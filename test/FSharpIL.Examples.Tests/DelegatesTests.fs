module FSharpIL.DelegatesTests

open Expecto
open FsCheck

open Swensen.Unquote

type MyInstance =
    { Function: string -> int32 -> string }
    member this.MyMethod(arg1: string, arg2: int32) = this.Function arg1 arg2

[<Tests>]
let tests =
    testList "delegates example" [
        testList "using static method" [
            testProperty "works with F# function" <| fun(arg1, arg2, f) ->
                let sut = MyDelegate f
                test <@ sut.Invoke(arg1, arg2) = f arg1 arg2 @>

            testProperty "duplicate string returns string with correct length" <| fun(NonNull str, PositiveInt times) ->
                let sut = MyDelegate(fun arg1 arg2 -> MyClass.DuplicateString(arg1, arg2))
                test <@ sut.Invoke(str, times).Length = str.Length * times @>
        ]

        testProperty "works with instance method" <| fun (instance: MyInstance, arg1, arg2) ->
            let sut = MyDelegate(fun arg1 arg2 -> instance.MyMethod(arg1, arg2))
            test <@ sut.Invoke(arg1, arg2) = instance.MyMethod(arg1, arg2) @>

        testProperty "example #2 uses indexof" <| fun(NonNull str, NonNull sub) ->
            let sut = MyClass.Example2 str
            test <@ sut.Invoke sub = str.IndexOf sub @>
    ]
