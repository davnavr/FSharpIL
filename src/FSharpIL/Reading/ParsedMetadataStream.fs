namespace FSharpIL.Reading

open System

open FSharpIL

[<NoComparison; NoEquality>]
type internal ParsedMetadataStream =
    { Chunk: ChunkReader
      mutable Buffer: byte[]
      StreamOffset: uint64
      StreamSize: uint64 }

    member this.AppendByte(i, value) =
        if i >= this.Buffer.Length then
            let buffer' = Array.zeroCreate<byte>(this.Buffer.Length * 2)
            Span(this.Buffer).CopyTo(Span buffer')
            this.Buffer <- buffer'
        this.Buffer.[i] <- value

    member this.IsValidOffset(Convert.U8 offset: uint32) = this.Chunk.IsValidOffset offset && offset < this.StreamSize

[<RequireQualifiedAccess>]
module internal ParsedMetadataStream =
    let [<Literal>] DefaultBufferSize = 32;

    let ofHeader
        roffset
        (chunk: ChunkReader)
        { ParsedStreamHeader.Offset = Convert.U8 offset; Size = Convert.U8 size }
        (stream: outref<_>) =
        if chunk.HasFreeBytes(offset, size) then
            stream <-
                { Chunk = chunk
                  StreamOffset = roffset + offset
                  StreamSize = size
                  Buffer = Array.zeroCreate DefaultBufferSize }
            true
        else false
