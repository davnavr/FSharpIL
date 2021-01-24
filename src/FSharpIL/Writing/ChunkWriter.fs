﻿namespace FSharpIL.Writing

open System
open System.Collections.Generic

open Microsoft.FSharp.Core.Operators.Checked

[<Sealed>]
type internal ChunkWriter (chunk: LinkedListNode<byte[]>, position: int) =
    do
        if chunk = null then nameof chunk |> nullArg

        if position < 0 || position >= chunk.Value.Length then
            sprintf
                "The initial position (%i) must be a valid index"
                position
            |> invalidArg (nameof position) 

    let mutable pos = position
    let mutable size = 0u
    let mutable current = chunk

    new (chunk) = ChunkWriter(chunk, 0)

    member _.Chunk = current
    /// Gets the number of bytes remaining in the current chunk.
    member _.FreeBytes = current.Value.Length - pos
    member _.Position = pos
    member _.Size = size

    member this.WriteU1 value =
        if pos >= current.Value.Length then
            current <- current.List.AddAfter(current, Array.zeroCreate<byte> current.Value.Length)
        this.Chunk.Value.[pos] <- value
        pos <- pos + 1
        size <- size + 1u

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
            let destination = Span(current.Value, pos, bytes.Length)
            Span(bytes).CopyTo destination
            pos <- pos + bytes.Length
            size <- size + uint32 bytes.Length
        else
            Array.toSeq bytes |> this.WriteBytes

    member this.WriteBytes(bytes: seq<byte>) = Seq.iter this.WriteU1 bytes

    member _.MoveToEnd() = pos <- current.Value.Length

    member _.ResetSize() = size <- 0u
