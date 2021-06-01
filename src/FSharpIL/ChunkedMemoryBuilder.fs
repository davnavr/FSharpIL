namespace FSharpIL

open System
open System.Collections.Generic
open System.Collections.Immutable

// TODO: Should the builder type be a struct or reference type?
/// Represents a mutable, non-contiguous region of memory split into chunks of equal sizes.
type ChunkedMemoryBuilder = struct
    val mutable private current: LinkedListNode<byte[]>
    val mutable private pos: int32

    internal new (current, position) = { current = current; pos = position }

    /// <param name="size">The size of each chunk in bytes.</param>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// The <paramref name="size"/> of each chunk is less than two.
    /// </exception>
    new (size) =
        if size <= 1 then
            raise(ArgumentOutOfRangeException("size", size, "The size of each chunk must be greater than one"))
        let chunks = LinkedList()
        ChunkedMemoryBuilder(chunks.AddFirst(Array.zeroCreate size), 0)

    member this.ChunkCount = this.current.List.Count
    member this.ChunkSize = this.current.Value.Length
    member this.FreeBytes = this.ChunkSize - this.pos
    member this.Position = this.pos

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
        this.pos <- this.pos + 1

    member this.Write(data: ReadOnlySpan<byte>) =
        let mutable i = 0
        while i < data.Length do
            this.CheckNextChunk()
            let length = min this.FreeBytes (data.Length - i)
            let destination = Span<_>(this.current.Value, this.pos, length)
            data.Slice(i, length).CopyTo destination
            i <- i + length
        this.pos <- this.pos + data.Length

    // TODO: Avoid code duplication with ByteWriterExtensions methods.
    member inline this.Write(data: Span<byte>) = this.Write(Span<byte>.op_Implicit data)
    member inline this.Write(data: byte[]) = this.Write(ReadOnlySpan data)
    member inline this.Write(data: ImmutableArray<byte>) = this.Write(data.AsSpan())

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
                this.pos <- this.pos + skipped
                remaining <- remaining - skipped

    member this.AlignTo alignment =
        if alignment < 0 then
            raise(ArgumentOutOfRangeException("alignment", alignment, "The alignment must not be negative"))
        elif alignment > 0 then
            this.SkipBytes((Round.upTo alignment this.pos) - this.pos)

    /// Moves the writer to the end of the current chunk.
    member this.MoveToEnd() = this.pos <- this.ChunkSize

    override this.ToString() =
        let free = this.FreeBytes
        let sb = System.Text.StringBuilder((free + 1) * 5)
        Printf.bprintf sb "(%i)" free
        for i = this.pos to this.current.Value.Length - 1 do
            Printf.bprintf sb " 0x%02X" this.current.Value.[i]
        sb.ToString()

    // TODO: Better name for method used to get data.
    member internal this.ToReadOnly() =
        let data = List this.ChunkCount
        let mutable chunk = this.current
        while chunk <> null do
            data.Add(ReadOnlyMemory chunk.Value)
            chunk <- chunk.Next
        data

    interface IByteWriter with member this.Write data = this.Write data
end
