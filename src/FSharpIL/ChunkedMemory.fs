namespace FSharpIL

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

/// Represents a non-contiguous region of memory split into chunks of equal sizes.
[<IsReadOnly;>]
[<CustomEquality; NoComparison>]
type ChunkedMemory = struct
    val private chunks: ImmutableArray<ImmutableArray<byte>>
    val private soffset: uint32
    /// The total length of this region of memory, in bytes.
    val Length: uint32
    internal new (chunks, start, length) = { chunks = chunks; soffset = start; Length = length }
    member this.IsEmpty = this.chunks.IsEmpty || this.chunks.[0].IsEmpty
    member this.ChunkCount = this.chunks.Length
    /// The length of each chunk except for the last chunk.
    member this.ChunkSize = if this.IsEmpty then 0 else this.chunks.[0].Length

    member internal this.GetIndex offset =
        let offset', csize' = offset + this.soffset, uint32 this.ChunkSize
        let chunki = offset' / csize'
        struct(int32 chunki, int32(offset' - chunki * csize'))

    member this.Item with get offset =
        let struct(chunki, i) = this.GetIndex offset
        this.chunks.[chunki].[i]

    member this.IsValidOffset offset = offset < this.Length
    member inline this.HasFreeBytes(offset: uint32, length) = this.IsValidOffset(offset + length - 1u)

    member private this.SlowCopyTo(offset, buffer: Span<byte>) =
        let mutable i = 0u
        while i < uint32 buffer.Length && i < this.Length do
            buffer.[int32 i] <- this.[offset + i]
            i <- i + 1u
        i

    /// <returns>
    /// <see langword="true"/> if a span could successfully be created from the chunk at the specified offset; otherwise
    /// <see langword="false"/>.
    /// </returns>
    member private this.TrySpanChunk(offset, buffer: outref<ReadOnlySpan<byte>>) =
        if this.IsValidOffset offset then
            let struct(chunki, i) = this.GetIndex offset
            buffer <- this.chunks.[chunki].AsSpan().Slice i
            true
        else false

    /// <summary>Attempts to copy the data starting at the specified <paramref name="offset"/> to the specified buffer.</summary>
    member this.TryCopyTo(offset, buffer: Span<byte>) =
        if this.chunks.IsEmpty || buffer.Length = 0 then true
        elif this.HasFreeBytes(offset, uint32 buffer.Length) then
            let mutable chunk = ReadOnlySpan()
            if this.TrySpanChunk(offset, &chunk) && chunk.Length >= buffer.Length then
                chunk.Slice(0, buffer.Length).CopyTo buffer // Copy without having to loop
            else
                this.SlowCopyTo(offset, buffer) |> ignore
            true
        else false

    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the destination <paramref name="buffer"/> contains more bytes than the number of bytes remaining at the
    /// specified <paramref name="offset"/>.
    /// </exception>
    member this.CopyTo(offset, buffer) =
        if not(this.TryCopyTo(offset, buffer)) then
            raise (
                ArgumentOutOfRangeException (
                    "offset",
                    offset,
                    sprintf
                        "The destination buffer contains more bytes (%i bytes) than the number of free bytes (%i bytes)."
                        buffer.Length
                        (this.Length - offset)
                )
            )

    interface IEquatable<ChunkedMemory> with
        member this.Equals other =
            this.chunks.Equals other.chunks && this.Length = other.Length && this.soffset = other.soffset
end

[<RequireQualifiedAccess>]
module ChunkedMemory =
    let empty = ChunkedMemory(ImmutableArray.Empty, 0u, 0u)

    /// Creates a non-contiguous region of memory, without checking that all chunks except the last chunk have the same length.
    let internal ofArrayUnsafe (chunks: byref<byte[][]>) =
        match chunks with
        | null
        | [||] -> empty
        | _ ->
            let last = chunks.Length - 1
            ChunkedMemory(Unsafe.As &chunks, 0u, (uint32 chunks.[0].Length * uint32 last) + uint32 chunks.[last].Length)

    let readU2 offset (chunks: inref<ChunkedMemory>) =
        let buffer = Span.stackalloc<byte> 2
        chunks.CopyTo(offset, buffer)
        Bytes.readU2 0 buffer

    let readU4 offset (chunks: inref<ChunkedMemory>) =
        let buffer = Span.stackalloc<byte> 4
        chunks.CopyTo(offset, buffer)
        Bytes.readU4 0 buffer

    let readU8 offset (chunks: inref<ChunkedMemory>) =
        let buffer = Span.stackalloc<byte> 8
        chunks.CopyTo(offset, buffer)
        Bytes.readU8 0 buffer

type ChunkedMemory with
    member this.TrySlice(start, length, slice: outref<ChunkedMemory>) =
        if length = 0u then
            slice <- ChunkedMemory.empty
            true
        elif this.HasFreeBytes(start, length) then
            slice <- ChunkedMemory(this.chunks, start, length)
            true
        else false

    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="start"/> offset or <c><paramref name="start"/> + <paramref name="length"/></c> exceeds
    /// the maximum valid offset.
    /// </exception>
    member this.Slice(start, length) =
        match this.TrySlice(start, length) with
        | false, _ ->
            raise (
                ArgumentOutOfRangeException(
                    (if this.IsValidOffset start then "length" else "start"),
                    start + length,
                    sprintf "The specified offset exceeds the maximum valid offset (%i)" this.Length
                )
            )
        | true, slice -> slice

    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="start"/> offset exceeds the maximum valid offset.
    /// </exception>
    member this.Slice start = this.Slice(start, this.Length - start)

    member this.ToImmutableArray() =
        match this.ChunkCount with
        | 0 -> ImmutableArray.Empty
        | 1 -> this.chunks.[0]
        | _ ->
            let length = int32 this.Length
            let mutable buffer = Array.zeroCreate<byte> length
            let mutable struct(chunki, i), remaining = this.GetIndex 0u, buffer.Length
            while remaining > 0 do
                let copied = min remaining (this.chunks.[chunki].Length - i)
                let destination = Span(buffer, length - remaining, copied)
                this.chunks.[chunki].AsSpan().Slice(i, copied).CopyTo destination
                i <- 0
                chunki <- chunki + 1
                remaining <- remaining - copied
            Unsafe.As<_, ImmutableArray<byte>> &buffer

    // TODO: Have better ways for testing equality.
    member this.Equals(array: ImmutableArray<byte>) =
        if this.Length = uint32 array.Length then
            let mutable equal, offset = true, 0u
            while equal && offset < this.Length do
                if this.[offset] <> array.[int32 offset] then
                    equal <- false
                offset <- offset + 1u
            equal
        else false

    member this.Equals(array: byte[]) =
        let mutable array' = array
        this.Equals(Unsafe.As<_, ImmutableArray<byte>> &array')
