namespace FSharpIL

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

/// Represents a non-contiguous region of memory.
[<IsReadOnly; Struct>]
[<CustomEquality; NoComparison>]
type Chunks =
    internal
        { Chunks: ImmutableArray<ImmutableArray<byte>>
          Size: uint32
          StartOffset: uint32 }

    member this.IsEmpty = this.Chunks.IsEmpty || this.Chunks.[0].IsEmpty
    member this.ChunkCount = this.Chunks.Length
    member this.ChunkSize = if this.IsEmpty then 0 else this.Chunks.[0].Length
    member this.ByteLength = this.Size

    member internal this.GetIndex offset =
        let offset', csize' = offset + this.StartOffset, uint32 this.ChunkSize
        let chunki = offset / csize'
        struct(int32 chunki, int32(offset' - chunki * csize'))

    member this.Item with get offset =
        let struct(chunki, i) = this.GetIndex offset
        this.Chunks.[chunki].[i]

    member this.Equals(array: ImmutableArray<byte>) =
        if this.Size = uint32 array.Length then
            let mutable equal, offset = true, 0u
            while equal && offset < this.Size do
                if this.[offset] <> array.[int32 offset] then
                    equal <- false
            equal
        else false

    interface IEquatable<Chunks> with
        // TODO: Check that this calls the static equals thing for ImmmutableArray
        member this.Equals other = this.Chunks.Equals other.Chunks && this.Size = other.Size && this.StartOffset = other.StartOffset

    interface IEquatable<ImmutableArray<byte>> with member this.Equals array = this.Equals array

[<RequireQualifiedAccess>]
module Chunks = // TODO: Since Chunks is greater than IntPtr.Size, consider using methods that accept inref instead.
    let empty = { Chunks = ImmutableArray.Empty; StartOffset = 0u; Size = 0u }

    let isValidOffset offset { StartOffset = starti; Size = len } = offset >= starti && offset < starti + len

    let inline hasFreeBytes (offset: uint32) (length: uint32) chunks = isValidOffset (offset + length - 1u) chunks

    let trySlice start length chunks =
        match length with
        | 0u -> ValueSome empty
        | _ when hasFreeBytes start length chunks ->
            ValueSome { chunks with StartOffset = start; Size = length }
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

    /// Creates a non-contiguous region of memory, without checking that all chunks except the last chunk have the same length.
    let internal ofArrayUnsafe (chunks: byref<byte[][]>) =
        match chunks with
        | null
        | [||] -> empty
        | _ ->
            let last = chunks.Length - 1
            { Chunks = Unsafe.As &chunks
              StartOffset = 0u
              Size = (uint32 chunks.[0].Length * uint32 last) + uint32 chunks.[last].Length }

    let internal slowCopyToSpan (chunks: Chunks) offset (buffer: Span<byte>) =
        for bufferi = 0 to buffer.Length - 1 do
            buffer.[bufferi] <- chunks.[offset + uint32 bufferi]

    let tryGetSpan (chunks: Chunks) offset length (buffer: outref<ReadOnlySpan<byte>>) =
        let struct(chunki, i) = chunks.GetIndex offset
        let chunk = chunks.Chunks.[chunki]
        if i + length <= chunk.Length then
            buffer <- chunk.AsSpan().Slice(0, length)
            true
        else false

    let copyToSpan chunks offset (buffer: Span<byte>) =
        let mutable source = ReadOnlySpan()
        if tryGetSpan chunks offset buffer.Length &source
        then source.CopyTo buffer // Copy without having to loop
        else slowCopyToSpan chunks offset buffer

    let tryCopyToSpan (chunks: Chunks) offset (buffer: Span<byte>) =
        if chunks.IsEmpty then true
        elif hasFreeBytes offset (uint32 buffer.Length) chunks then
            copyToSpan chunks offset buffer
            true
        else false

    let toBlock (chunks: Chunks) =
        match chunks.ChunkCount with
        | 0
        | 1 -> chunks.Chunks.[0]
        | _ ->
            let mutable buffer = Array.zeroCreate<byte>(int32 chunks.Size)
            for i = 0 to chunks.ChunkCount - 1 do
                chunks.Chunks.[i].CopyTo(buffer, i * chunks.ChunkSize)
            Unsafe.As<_, ImmutableArray<byte>> &buffer


