namespace rec FSharpIL

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

/// Represents a non-contiguous region of memory.
[<IsReadOnly; Struct>]
type Chunks =
    internal
        { Chunks: byte[][]
          StartOffset: uint32
          EndOffset: uint32 }

    member this.IsEmpty =
        match this.Chunks with
        | null | [||] -> true
        | _ -> false
    member this.ChunkCount = if this.IsEmpty then 0 else this.Chunks.Length
    member this.ChunkSize = if this.IsEmpty then 0 else this.Chunks.[0].Length
    member this.ByteLength = if this.IsEmpty then 0u else this.StartOffset - this.EndOffset
    member inline this.Item with get offset = Chunks.read offset this
    member this.GetEnumerator() = new ChunksEnumerator(this)

type ChunksEnumerator = struct
    val private chunks: Chunks
    val mutable private offset: uint32
    val mutable private init: bool
    new (chunks) = { chunks = chunks; offset = 0u; init = false }

    member inline private this.ValidOffset = this.offset <= this.chunks.EndOffset

    member this.Current = 
        if not this.init then
            invalidOp "The enumerator has not yet been initialized"
        if not this.ValidOffset then
            invalidOp "The end of the collection has been reached"
        this.chunks.[this.offset]

    member this.MoveNext() =
        if not this.init then
            this.init <- true
        else
            this.offset <- this.offset + 1u
        this.ValidOffset

    interface IEnumerator<byte> with
        member this.Current = this.Current
        member this.Current = box this.Current
        member this.Reset() =
            this.offset <- 0u
            this.init <- false
        member this.MoveNext() = this.MoveNext()
        member _.Dispose() = ()
end

[<RequireQualifiedAccess>]
module Chunks =
    let empty = { Chunks = Array.empty; StartOffset = 0u; EndOffset = 0u }

    let private getIndex offset chunks =
        let offset', csize' = offset + chunks.StartOffset, uint32 chunks.ChunkSize
        let chunki = offset / csize'
        let i = int32(offset' - chunki * csize')
        struct(int32 chunki, i)

    let read offset chunks =
        let struct(chunki, i) = getIndex offset chunks
        chunks.Chunks.[chunki].[i]

    let isValidOffset offset { StartOffset = starti; EndOffset = endi } = offset >= starti && offset <= endi

    let inline hasFreeBytes (offset: uint32) (length: uint32) chunks = isValidOffset (offset + length - 1u) chunks

    let trySlice start length chunks =
        match length with
        | 0u -> ValueSome empty
        | _ when hasFreeBytes start length chunks ->
            ValueSome { chunks with StartOffset = start; EndOffset = start + length - 1u }
        | _ -> ValueNone

    /// <exception cref="T:System.ArgumentException">
    /// Thrown when the <paramref name="start"/> offset or when the sum of the <paramref name="start"/> and the offset
    /// <paramref name="length"/> exceeds the maximum valid offset.
    /// </exception>
    let slice start length chunks =
        match trySlice start length chunks with
        | ValueSome result -> result
        | ValueNone ->
            invalidArg
                (if isValidOffset start chunks then "length" else "start")
                "The offset (%i) exceeds the maximum valid offset into the chunks"

    let inline internal ofArrayUnsafe chunks = { Chunks = chunks; StartOffset = 0u; EndOffset = 0u }

    let internal slowCopyToSpan chunks offset (buffer: Span<byte>) =
        for bufferi = 0 to buffer.Length - 1 do
            buffer.[bufferi] <- read (offset + uint32 bufferi) chunks

    let tryGetSpan chunks offset length (buffer: outref<Span<byte>>) =
        let struct(chunki, i) = getIndex offset chunks
        let chunk = chunks.Chunks.[chunki]
        if i + length <= chunk.Length then
            buffer <- Span(chunk, i, length)
            true
        else false

    let copyToSpan chunks offset (buffer: Span<byte>) =
        let mutable source = Span()
        if tryGetSpan chunks offset buffer.Length &source
        then source.CopyTo buffer // Copy without having to loop
        else slowCopyToSpan chunks offset buffer
