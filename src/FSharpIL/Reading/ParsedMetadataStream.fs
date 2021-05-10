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

    /// Reads a variable-width, unsigned big-endian integer (II.23.2).
    let tryReadUnsigned offset { Chunk = chunk; StreamOffset = soffset } (value: outref<uint32>) =
        let offset' = offset + soffset
        let parsed = chunk.ReadU1 offset'
        if parsed &&& 0b1000_0000uy = 0uy then // 1 byte
            value <- uint32(parsed &&& 0b0111_1111uy)
            Ok 1uy
        elif parsed &&& 0b1100_0000uy = 0b1000_0000uy then // 2 bytes
            value <- (uint16(parsed &&& 0b0011_1111uy) <<< 8) + uint16(chunk.ReadU1(offset' + 1UL)) |> uint32
            Ok 2uy
        elif parsed &&& 0b1110_0000uy = 0b1100_0000uy then // 4 bytes
            value <-
                (uint32(parsed &&& 0b0001_1111uy) <<< 24)
                + (uint32(chunk.ReadU1(offset' + 1UL)) <<< 16)
                + (uint32(chunk.ReadU1(offset' + 2UL)) <<< 8)
                + uint32(chunk.ReadU1(offset' + 3UL))
            Ok 4uy
        else Error parsed
