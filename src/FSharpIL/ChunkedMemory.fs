namespace FSharpIL

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Utilities

[<IsReadOnly; Struct>]
type internal ChunkedMemoryIndex =
    { /// Index into the list of chunks.
      ListIndex: int32
      ChunkIndex: int32 }

/// Represents a non-contiguous region of memory split into chunks of equal sizes.
[<IsReadOnly>]
[<CustomEquality; NoComparison>]
type ChunkedMemory = struct
    val private chunks: ImmutableArray<ImmutableArray<byte>>
    val private soffset: uint32
    /// The total length of this region of memory, in bytes.
    val Length: uint32

    internal new (chunks, start, length) = { chunks = chunks; soffset = start; Length = length }

    internal new (chunks: ImmutableArray<ImmutableArray<byte>>) =
        let length =
            if chunks.IsDefaultOrEmpty
            then 0u
            else
                let chunkl = uint32 chunks.[0].Length
                if chunks.Length = 1
                then chunkl
                else ((uint32 chunks.Length - 1u) * chunkl) + uint32 chunks.[chunks.Length - 1].Length
        ChunkedMemory(chunks, 0u, length)

    member this.IsEmpty = this.chunks.IsDefaultOrEmpty || this.chunks.[0].IsDefaultOrEmpty
    member this.ChunkCount = this.chunks.Length
    /// The length of each chunk except for the last chunk.
    member this.ChunkSize = if this.IsEmpty then 0 else this.chunks.[0].Length

    member internal this.GetIndex offset =
        if this.IsEmpty then invalidOp(sprintf "Cannot access offset 0x%08X, the memory is empty" offset)
        let offset', csize' = offset + this.soffset, uint32 this.ChunkSize
        let chunki = offset' / csize'
        { ListIndex = int32 chunki; ChunkIndex = int32(offset' - chunki * csize') }

    /// <summary>Gets the value of the byte at the specified offset.</summary>
    /// <exception cref="T:System.InvalidOperationException">Thrown when the region of memory is empty.</exception>
    member this.Item with get offset =
        let index = this.GetIndex offset
        this.chunks.[index.ListIndex].[index.ChunkIndex]

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
            let index = this.GetIndex offset
            buffer <- this.chunks.[index.ListIndex].AsSpan().Slice index.ChunkIndex
            true
        else false

    member internal this.UnsafeCopyTo(offset, buffer: Span<byte>) =
        let mutable chunk = ReadOnlySpan()
        if this.TrySpanChunk(offset, &chunk) && chunk.Length >= buffer.Length then
            chunk.Slice(0, buffer.Length).CopyTo buffer // Copy without having to loop
        else
            this.SlowCopyTo(offset, buffer) |> ignore

    /// <summary>Attempts to copy the data starting at the specified <paramref name="offset"/> to the specified buffer.</summary>
    member this.TryCopyTo(offset, buffer: Span<byte>) =
        if this.chunks.IsEmpty || buffer.Length = 0 then true
        elif this.HasFreeBytes(offset, uint32 buffer.Length) then
            this.UnsafeCopyTo(offset, buffer)
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
            if this.soffset = other.soffset && this.Length = other.Length then
                noImpl "equality for chunked memory"
            else false
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

    let tryReadU2 offset (memory: inref<ChunkedMemory>) =
        let buffer = Span.stackalloc<byte> 2
        if memory.TryCopyTo(offset, buffer)
        then ValueSome(Bytes.toU2 0 buffer)
        else ValueNone

    let tryReadU4 offset (memory: inref<ChunkedMemory>) =
        let buffer = Span.stackalloc<byte> 4
        if memory.TryCopyTo(offset, buffer)
        then ValueSome(Bytes.toU4 0 buffer)
        else ValueNone

    let tryReadU8 offset (memory: inref<ChunkedMemory>) =
        let buffer = Span.stackalloc<byte> 8
        if memory.TryCopyTo(offset, buffer)
        then ValueSome(Bytes.toU8 0 buffer)
        else ValueNone

    let inline readU2 offset (memory: inref<_>) = (tryReadU2 offset &memory).Value
    let inline readU4 offset (memory: inref<_>) = (tryReadU4 offset &memory).Value
    let inline readU8 offset (memory: inref<_>) = (tryReadU8 offset &memory).Value

    // TODO: Make BigEndian versions if necessary.
    //module BigEndian

type ChunkedMemory with
    member this.TrySlice(start, length, slice: outref<ChunkedMemory>) =
        if length = 0u then
            slice <- ChunkedMemory.empty
            true
        elif this.HasFreeBytes(start, length) then
            slice <- ChunkedMemory(this.chunks, this.soffset + start, length)
            true
        else false

    member inline this.TrySlice(start, slice: outref<ChunkedMemory>) = this.TrySlice(start, this.Length - start, &slice)

    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="start"/> offset or <c>start + length</c> exceeds
    /// the maximum valid offset.
    /// </exception>
    member this.Slice(start, length: uint32) =
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
    member inline this.Slice start = this.Slice(start, this.Length - start)

    member this.AsMemoryArray() =
        match this.ChunkCount with
        | 0 -> ImmutableArray<_>.Empty
        | 1 -> ImmutableArray.Create(this.chunks.[0].AsMemory().Slice(int32 this.soffset, int32 this.Length))
        | _ ->
            let starti = this.GetIndex 0u
            let endi = this.GetIndex this.Length
            let mutable chunks' = Array.zeroCreate<ReadOnlyMemory<byte>>(endi.ListIndex - starti.ListIndex + 1)
            let mutable remaining = this.Length
            for chunki = starti.ListIndex to endi.ListIndex do
                let current = &this.chunks.ItemRef chunki
                let start = if chunki = starti.ListIndex then starti.ChunkIndex else 0
                let length = min remaining (uint32 current.Length)
                chunks'.[chunki - starti.ListIndex] <- current.AsMemory().Slice(start, int32 length)
                remaining <- remaining - length
            Unsafe.As &chunks'

    member this.ToImmutableArray() =
        let mutable buffer, i = Array.zeroCreate<byte>(int32 this.Length), 0
        for chunk in this.AsMemoryArray() do
            let length = min buffer.Length chunk.Length
            chunk.Span.Slice(0, length).CopyTo(Span(buffer, i, length))
            i <- i + length
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

    member this.Equals(array: byte[]) = this.Equals(Convert.unsafeTo<_, ImmutableArray<byte>> array)
