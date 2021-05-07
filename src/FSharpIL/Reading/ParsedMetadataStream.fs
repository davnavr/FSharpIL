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

    let ofHeader (chunk: ChunkReader) { ParsedStreamHeader.Offset = Convert.U8 offset; Size = Convert.U8 size } =
        if chunk.HasFreeBytes(offset, size) then
            Ok { Chunk = chunk; StreamOffset = offset; StreamSize = size; Buffer = Array.zeroCreate DefaultBufferSize }
        else Error size
