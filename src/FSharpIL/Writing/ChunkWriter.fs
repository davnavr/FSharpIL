namespace FSharpIL.Writing

open System

open Microsoft.FSharp.Core.Operators.Checked

[<Sealed>]
type internal ChunkWriter (chunk: Chunk, position: int) = // TODO: How to modify size stack in chunk list?
    do
        if position < 0 || position >= chunk.Data.Length then
            sprintf
                "The initial position (%i) must be a valid index"
                position
            |> invalidArg (nameof position) 

    let mutable pos = position
    let mutable current = chunk

    new (chunk) = ChunkWriter(chunk, 0)

    member _.Chunk = current
    /// Gets the number of bytes remaining in the current chunk.
    member _.FreeBytes = current.Data.Length - pos
    member _.Position = pos

    member this.WriteU1 value =
        if pos >= current.Data.Length then
            current <- current.List.AddAfter(current, Array.zeroCreate<byte> current.Data.Length)
        this.Chunk.Data.[pos] <- value
        pos <- pos + 1
        chunk.List.IncrementSize 1u

    /// Writes an unsigned 2-byte integer in little-endian format.
    member this.WriteU2 value =
        this.WriteU1(value &&& 0xFFus |> byte)
        this.WriteU1((value >>> 8) &&& 0xFFus |> byte)

    member inline this.WriteU2 value = this.WriteU2(uint16 value)

    /// Writes an unsigned 4-byte integer in little-endian format.
    member this.WriteU4 value =
        this.WriteU1(value &&& 0xFFu |> byte)
        this.WriteU1((value >>> 8) &&& 0xFFu |> byte)
        this.WriteU1((value >>> 16) &&& 0xFFu |> byte)
        this.WriteU1((value >>> 24) &&& 0xFFu |> byte)

    member inline this.WriteU4 value = this.WriteU4(uint32 value)

    /// Writes an unsigned 8-byte integer in little-endian format.
    member this.WriteU8 value =
        this.WriteU1(value &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 8) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 16) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 24) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 32) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 40) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 48) &&& 0xFFUL |> byte)
        this.WriteU1((value >>> 56) &&& 0xFFUL |> byte)

    member inline this.WriteU8 value = this.WriteU8(uint64 value)

    member this.WriteBytes(bytes: byte[]) =
        if this.FreeBytes >= bytes.Length then
            let destination = Span(current.Data, pos, bytes.Length)
            Span(bytes).CopyTo destination
            pos <- pos + bytes.Length
            chunk.List.IncrementSize (uint32 bytes.Length)
        else
            Array.toSeq bytes |> this.WriteBytes

    member this.WriteBytes(bytes: seq<byte>) = Seq.iter this.WriteU1 bytes

    member _.MoveToEnd() =
        chunk.List.IncrementSize(current.Data.Length - pos |> uint32)
        pos <- current.Data.Length
