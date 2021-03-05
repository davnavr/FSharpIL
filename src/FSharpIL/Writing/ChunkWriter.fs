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

    member private this.HasFreeBytes length = this.FreeBytes >= length

    member _.CreateWriter() =
        ChunkWriter(current, pos, defaultCapacity)

    member this.WriteBytes(bytes: byte[]) =
        if this.HasFreeBytes bytes.Length then
            let destination = Span(current.Value, pos, bytes.Length)
            Span(bytes).CopyTo destination
            pos <- pos + bytes.Length
            size <- size + uint32 bytes.Length
        else
            Array.toSeq bytes |> this.WriteBytes

    member this.WriteBytes(bytes: seq<byte>) = Seq.iter this.WriteU1 bytes

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
