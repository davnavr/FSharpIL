module FSharpIL.CollectionTests

open Expecto
open FsCheck

open Swensen.Unquote

open System

open Example

[<Tests>]
let tests =
    testList "fraction" [
        testProperty "adding elements to empty list is successful" <| fun(PositiveInt capacity, NonNull items) ->
            let sut = MyCollection<int32> capacity
            Array.iter sut.Add items
            test <@ sut.ToArray() = items @>

        fun(PositiveInt capacity, NonNull(strings: string[])) ->
            let sut = MyCollection<obj> capacity
            Array.iter sut.Add strings
            test <@ sut.Cast<string>().ToArray() = strings @>
        |> testProperty "casting object collection to string collection is successful"
    ]
