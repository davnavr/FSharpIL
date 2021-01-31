﻿namespace FSharpIL.Writing

open System

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Bytes

// TODO: Determine if this should be made into a struct or ref stuct, or should remain a reference type.
[<AllowNullLiteral>]
[<Sealed>]
type internal ChunkWriter (chunk: Chunk, position: int, defaultCapacity: int32) =
    do
        if defaultCapacity <= 0 then
            sprintf
                "The default capacity (%i) must be a positive integer"
                defaultCapacity
            |> invalidArg (nameof defaultCapacity)

        if position < 0 || position > chunk.Data.Length then
            sprintf
                "The initial position (%i) must be a valid index"
                position
            |> invalidArg (nameof position)

    let mutable pos = position
    let mutable current = chunk
    let mutable size = 0u

    new (chunk, position) = ChunkWriter(chunk, position, chunk.Data.Length)
    new (chunk) = ChunkWriter(chunk, 0)

    member _.Chunk = current
    /// Gets the number of bytes remaining in the current chunk.
    member _.FreeBytes = current.Data.Length - pos
    member _.Position = pos
    member _.Size = size

    member this.WriteU1 value =
        if pos >= current.Data.Length then
            current <- current.List.AddAfter(current, Array.zeroCreate<byte> defaultCapacity)
            pos <- 0
        this.Chunk.Data.[pos] <- value
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

    member this.WriteBytes(bytes: byte[]) =
        if this.FreeBytes >= bytes.Length then
            let destination = Span(current.Data, pos, bytes.Length)
            Span(bytes).CopyTo destination
            pos <- pos + bytes.Length
            size <- size + uint32 bytes.Length
        else
            Array.toSeq bytes |> this.WriteBytes

    member this.WriteBytes(bytes: seq<byte>) = Seq.iter this.WriteU1 bytes

    member _.MoveToEnd() =
        pos <- current.Data.Length
        size <- uint32 current.Data.Length

    static member After(chunk: Chunk, defaultCapacity: int32) = ChunkWriter(chunk, chunk.Data.Length, defaultCapacity)
