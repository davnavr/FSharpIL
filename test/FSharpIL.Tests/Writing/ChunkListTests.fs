module FSharpIL.Writing.ChunkListTests

open Expecto

open FsCheck

let private (|Chunks|) = Array.map (fun (NonEmptyArray chunk) -> chunk: byte[])

[<Tests>]
let tests =
    testList "writing.chunk list" [
        testCase "empty list has no head or tail"  <| fun() ->
            let empty = ChunkList()
            Expect.isNone empty.Head "empty list should not have a head"
            Expect.isNone empty.Head "empty list should not have a tail"

        testProperty "add first results in list with same head and tail" <| fun (NonEmptyArray chunk) ->
            let sut = ChunkList()
            sut.AddFirst chunk |> ignore
            sut.Head = sut.Tail

        testProperty "add last results in list with same head and tail" <| fun (NonEmptyArray chunk) ->
            let sut = ChunkList()
            sut.AddLast chunk |> ignore
            sut.Head = sut.Tail

        testProperty "list with chunks added last should match sequence" <| fun (Chunks chunks) ->
            let sut = ChunkList()
            Array.iter (sut.AddLast >> ignore) chunks
            let actual = Seq.map (fun chunk -> chunk.Data) sut
            Expect.sequenceEqual actual chunks "chunks should match"

        testProperty "add after head should result in list with correct number of elements" <| fun (NonEmptyArray head, Chunks chunks) ->
            let sut = ChunkList()
            let head' = sut.AddFirst head
            Array.iter (fun chunk -> sut.AddAfter(head', chunk) |> ignore) chunks
            1u + uint32 chunks.Length = sut.Count

        testProperty "chunk added after tail should result in new tail" <| fun (NonEmptyArray newTail) ->
            let sut = ChunkList()
            sut.AddFirst [| 1uy |] |> ignore
            sut.AddLast [| 2uy |] |> ignore

            sut.AddAfter(sut.Tail.Value, newTail) = sut.Tail.Value
    ]
