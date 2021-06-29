namespace FSharpIL

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Utilities

// TODO: Should the builder type be a struct or reference type?
/// Builds a non-contiguous region of memory split into chunks of equal sizes.
type ChunkedMemoryBuilder = struct
    val mutable private current: LinkedListNode<byte[]>
    val mutable private pos: int32
    val mutable private length: uint32

    internal new (current, position, length) = { current = current; pos = position; length = length }

    /// <param name="size">The size of each chunk in bytes.</param>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// The <paramref name="size"/> of each chunk is less than two.
    /// </exception>
    new (size) =
        if size <= 1 then argOutOfRange "size" size "The size of each chunk must be greater than one"
        let chunks = LinkedList()
        ChunkedMemoryBuilder(chunks.AddFirst(Array.zeroCreate size), 0, 0u)

    member this.ChunkCount = this.current.List.Count
    member this.ChunkPosition = this.pos
    member this.ChunkSize = this.current.Value.Length
    member this.FreeBytes = this.ChunkSize - this.pos
    member this.Length = this.length

    member inline private this.IncrementPosition count =
        this.pos <- this.pos + count
        this.length <- this.length + uint32 count

    member private this.CheckNextChunk() =
        if this.pos >= this.current.Value.Length then
            this.current <-
                match this.current.Next with
                | null -> this.current.List.AddAfter(this.current, Array.zeroCreate this.ChunkSize)
                | existing -> existing
            this.pos <- 0

    member this.Write(value: uint8) =
        this.CheckNextChunk()
        this.current.Value.[this.pos] <- value
        this.IncrementPosition 1

    member this.Write(data: ReadOnlySpan<byte>) =
        let mutable i = 0
        while i < data.Length do
            this.CheckNextChunk()
            let length = min this.FreeBytes (data.Length - i)
            let destination = Span<_>(this.current.Value, this.pos, length)
            data.Slice(i, length).CopyTo destination
            i <- i + length
        this.IncrementPosition data.Length

    // TODO: Avoid code duplication with ByteWriterExtensions methods.
    member inline this.Write(data: Span<byte>) = this.Write(Span<byte>.op_Implicit data)
    member inline this.Write(data: byte[]) = this.Write(ReadOnlySpan data)
    member inline this.Write(data: ImmutableArray<byte>) = this.Write(data.AsSpan())
    member inline this.Write(data: ReadOnlyMemory<byte>) = this.Write data.Span

    member this.Write(data: ChunkedMemory) = for chunk in data.AsMemoryArray() do this.Write chunk

    /// Writes an unsigned 2-byte integer in little-endian format.
    member this.WriteLE(value: uint16) = ByteWriterExtensions.WriteLE(&this, value)
    /// Writes an unsigned 4-byte integer in little-endian format.
    member this.WriteLE(value: uint32) = ByteWriterExtensions.WriteLE(&this, value)
    /// Writes an unsigned 8-byte integer in little-endian format.
    member this.WriteLE(value: uint64) = ByteWriterExtensions.WriteLE(&this, value)

    member this.SkipBytes count =
        if count > 0 then
            let mutable remaining = count
            while remaining > 0 do
                this.CheckNextChunk()
                let skipped = min this.FreeBytes remaining
                this.IncrementPosition skipped
                remaining <- remaining - skipped

    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="alignment"/> is negative.
    /// </exception>
    member this.AlignTo alignment =
        if alignment < 0 then argOutOfRange "alignment" alignment "The alignment must not be negative"
        elif alignment > 0 then this.SkipBytes((Round.upTo alignment this.pos) - this.pos)

    /// Moves the writer to the end of the current chunk.
    member this.MoveToEnd() =
        this.length <- this.length + uint32(this.ChunkSize - this.pos)
        this.pos <- this.ChunkSize

    override this.ToString() =
        let free = this.FreeBytes
        let sb = System.Text.StringBuilder((free + 1) * 5)
        Printf.bprintf sb "(%i)" free
        for i = this.pos to this.current.Value.Length - 1 do
            Printf.bprintf sb " 0x%02X" this.current.Value.[i]
        sb.ToString()

    /// Copies the contents of this builder to a new non-contiguous region of memory.
    member this.ToImmutable() =
        let mutable chunki = Checked.int32(uint32 this.pos - this.length)
        let mutable remaining, chunk, resulti = this.length, this.current, 0

        let chunkl = uint32 chunk.Value.Length

        let mutable results =
            let chunks = remaining / chunkl
            int32(if remaining % chunkl = 0u then chunks else chunks + 1u) |> Array.zeroCreate

        while remaining > 0u do
            let chunk' = Array.zeroCreate(if remaining > chunkl then int32 chunkl else int32 remaining)
            results.[resulti] <- chunk'
            let mutable chunki' = 0
            while chunki' < chunk'.Length do
                let length = min (chunk'.Length - chunki') (chunk.Value.Length - chunki)
                Span(chunk.Value, chunki, length).CopyTo(Span(chunk', chunki', length))
                chunki' <- chunki' + length
                chunki <- chunki + length
                if chunki >= chunk.Value.Length then
                    chunki <- 0
                    chunk <- chunk.Next
            remaining <- remaining - uint32 chunk'.Length
            resulti <- resulti + 1

        ChunkedMemory(Unsafe.As &results, 0u, this.length)

    member this.ReserveBytes count =
        let clone = ChunkedMemoryBuilder(this.current, this.pos, 0u)
        this.SkipBytes count
        clone

    interface IByteWriter with member this.Write data = this.Write data
end
