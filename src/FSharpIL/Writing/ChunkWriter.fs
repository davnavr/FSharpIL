namespace FSharpIL.Writing

open System
open System.Collections.Generic

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Bytes

// TODO: Since only append operations are used, consider something other than LinkedList such as FSharpList.
// TODO: See if making this a struct or a ref struct is a performance improvement.
[<Sealed>]
type internal ChunkWriter (chunk: LinkedListNode<byte[]>, position: int, defaultCapacity: int32) =
    do if position < 0 || position > chunk.Value.Length then
        sprintf
            "The initial position (%i) must be a valid index"
            position
        |> invalidArg (nameof position)

    let mutable current = chunk
    let mutable pos = position
    let mutable size = 0u

    new (chunk, position) = ChunkWriter(chunk, position, chunk.Value.Length)
    new (chunk) = ChunkWriter(chunk, 0)

    member _.Chunk = current
    /// Gets the number of bytes remaining in the current chunk.
    member _.FreeBytes = current.Value.Length - pos
    member _.Position = pos
    member _.Size = size

    member private _.CheckNextChunk() =
        if pos >= current.Value.Length then
            current <-
                match current.Next with
                | null -> current.List.AddAfter(current, Array.zeroCreate defaultCapacity)
                | existing -> existing
            pos <- 0

    member this.WriteU1 value =
        this.CheckNextChunk()
        this.Chunk.Value.[pos] <- value
        pos <- pos + 1
        size <- size + 1u

    member inline this.WriteU1 value = this.WriteU1(uint8 value)

    /// Writes an unsigned 2-byte integer in little-endian format.
    member this.WriteU2 (U2 (msb, lsb)) =
        this.WriteU1 lsb
        this.WriteU1 msb

    member inline this.WriteU2 value = this.WriteU2(uint16 value)

    /// Writes an unsigned 4-byte integer in little-endian format.
    member this.WriteU4 (U4 (msb, b3, b2, lsb)) =
        this.WriteU1 lsb
        this.WriteU1 b2
        this.WriteU1 b3
        this.WriteU1 msb

    member inline this.WriteU4 value = this.WriteU4(uint32 value)

    /// Writes an unsigned 8-byte integer in little-endian format.
    member this.WriteU8 (U8 (msb, b7, b6, b5, b4, b3, b2, lsb)) =
        this.WriteU1 lsb
        this.WriteU1 b2
        this.WriteU1 b3
        this.WriteU1 b4
        this.WriteU1 b5
        this.WriteU1 b6
        this.WriteU1 b7
        this.WriteU1 msb

    member inline this.WriteU8 value = this.WriteU8(uint64 value)

    member _.CreateWriter() = ChunkWriter(current, pos, defaultCapacity)

    member this.WriteBytes(bytes: Span<byte>) =
        let mutable i = 0
        while i < bytes.Length do
            this.CheckNextChunk()
            let length = min this.FreeBytes (bytes.Length - i)
            let destination = Span<_>(current.Value, pos, length)
            bytes.Slice(i).CopyTo destination
            i <- i + length
            pos <- pos + length
            size <- size + uint32 length

    member this.WriteBytes(bytes: byte[]) = this.WriteBytes(Span<_> bytes)
    member this.WriteBytes(bytes: seq<byte>) = Seq.iter this.WriteU1 bytes

    [<Obsolete>]
    /// <param name="length">The number of bytes to write from the chunk list.</param>
    member this.WriteBytes(chunks: LinkedList<byte[]>, length: uint32) =
        use mutable content = chunks.GetEnumerator()
        let mutable size' = length
        while size' > 0u && content.MoveNext() do
            let current = content.Current
            let currentLength = uint32 current.Length
            if size' > currentLength then
                size' <- size' - currentLength
                this.WriteBytes current
            else
                let remaining = Seq.take (int size') current
                size' <- 0u
                this.WriteBytes remaining
        if size' > 0u then
            sprintf
                "Unable to write byte chunks of length %i, chunk list unexpectedly ended with %i bytes remaining"
                length
                size'
            |> invalidOp

    member this.WriteBytes(startChunk: LinkedListNode<byte[]>, chunkIndex: int32, length: uint32) =
        let mutable chunk, remaining, index = startChunk, length, chunkIndex
        while remaining > 0u do
            if chunk = null then
                failwithf
                    "Unable to write byte chunks of length %i, chunk list unexpectedly ended with %i bytes remaining"
                    length
                    remaining
            let content = chunk.Value
            let length' = content.Length - index
            let source = Span<_>(content, index, length')
            this.WriteBytes source
            index <- index + length'
            remaining <- remaining + uint32 length'
            if index = content.Length then
                chunk <- chunk.Next
                index <- 0
        struct(chunk, index)

    member this.SkipBytes count =
        size <- size + count

        if count <= uint32 this.FreeBytes then
            pos <- pos + int32 count
        else
            let mutable remaining = count
            while remaining > 0u do
                this.CheckNextChunk()
                let skipped = min (uint32 this.FreeBytes) remaining
                pos <- pos + (int32 skipped)
                remaining <- remaining - skipped

    member this.AlignTo alignment =
        let pos' = uint32 pos
        let rounded = FSharpIL.Round.upTo alignment pos'
        this.SkipBytes(rounded - pos')

    /// Moves the writer to the end of the current chunk.
    member _.MoveToEnd() =
        let length = current.Value.Length
        size <- size + uint32(length - pos)
        pos <- length

    member _.ResetSize() = size <- 0u

    override this.ToString() =
        let free = this.FreeBytes
        let builder = System.Text.StringBuilder(5 + (free * 5))
        let chunk = current.Value
        builder.AppendFormat("({0})", free) |> ignore
        
        for i = pos to chunk.Length - 1 do
            builder.AppendFormat(" 0x{0:X2}", chunk.[i]) |> ignore
        
        builder.ToString()
