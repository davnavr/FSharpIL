namespace FSharpIL.Reading

open System

open FSharpIL

type internal ParsedMetadataStream internal (chunk: ChunkedMemory) =
    let [<Literal>] DefaultBufferSize = 16
    let mutable buffer = Array.zeroCreate DefaultBufferSize

    member _.Chunk = chunk
    member _.StreamSize = chunk.Length
    member _.Buffer = buffer

    member this.AppendByte(i, value) =
        if i >= buffer.Length then
            let buffer' = Array.zeroCreate<byte>(buffer.Length * 2)
            Span(buffer).CopyTo(Span buffer')
            buffer <- buffer'
        this.Buffer.[i] <- value

    member _.IsValidOffset offset = chunk.IsValidOffset offset

[<RequireQualifiedAccess>]
module internal ParsedMetadataStream =
    let ofHeader
        metadataRootOffset
        (chunk: inref<ChunkedMemory>)
        { ParsedStreamHeader.Offset = offset; Size = size }
        (stream: outref<_>) =
        match chunk.TrySlice(metadataRootOffset + offset, size) with
        | true, chunk' ->
            stream <- ParsedMetadataStream chunk'
            true
        | false, _ -> false
