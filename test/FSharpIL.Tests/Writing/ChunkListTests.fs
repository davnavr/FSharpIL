module FSharpIL.Writing.ChunkListTests

open Expecto

open FsCheck

[<Tests>]
let tests =
    testList "writing.chunk list" [
        testCase "empty list has no head or tail"  <| fun() ->
            let empty = ChunkList()
            Expect.isNone empty.Head "empty list should not have a head"
            Expect.isNone empty.Head "empty list should not have a tail"

        testProperty "list with one chunk added first should have same head and tail" <| fun (NonEmptyArray chunk) ->
            let sut = ChunkList()
            sut.AddFirst chunk |> ignore
            sut.Head = sut.Tail

        testProperty "list with one chunk added last should have same head and tail" <| fun (NonEmptyArray chunk) ->
            let sut = ChunkList()
            sut.AddLast chunk |> ignore
            sut.Head = sut.Tail

        testProperty "list with chunks added last should match sequence" <| fun chunks ->
            let sut = ChunkList()
            let expected = Array.map (fun (NonEmptyArray chunk) -> chunk) chunks
            Array.iter (sut.AddLast >> ignore) expected
            let actual = Seq.map (fun chunk -> chunk.Data) sut
            Expect.sequenceEqual actual expected "chunks should match"
    ]
