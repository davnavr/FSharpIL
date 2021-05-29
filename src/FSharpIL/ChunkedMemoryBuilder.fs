namespace FSharpIL

open System
open System.Collections.Generic

[<Sealed>]
type ChunkedMemoryBuilder (chunkSize: int32) =
    do if chunkSize <= 1 then raise(System.ArgumentOutOfRangeException("chunkSize", chunkSize, "The size of each chunk must be greater than one"))
    let chunks = LinkedList<byte[]>()
    member _.ChunkCount = chunks.Count
    member _.ChunkSize = chunkSize
