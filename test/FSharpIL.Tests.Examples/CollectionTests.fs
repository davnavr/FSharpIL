module FSharpIL.CollectionTests

open Expecto
open FsCheck

open Swensen.Unquote

open Example

[<Tests>]
let tests =
    testList "fraction" [
        testProperty "adding elements to empty list is successful" <| fun(PositiveInt capacity, NonNull items) ->
            let sut = MyCollection<int32>(int32 capacity)
            Array.iter sut.Add items
            test <@ sut.ToArray() = items @>
    ]
