namespace FSharpIL.Reading

open System

open FSharpIL

[<Sealed>]
type internal ChunkReader (csize: int32, chunks: byte[][]) =
    member _.GetIndex(offset: uint64) =
        let csize' = uint64 csize
        let chunki = offset / csize'
        let i = int32(offset - chunki * csize')
        struct(int32 chunki, i)

    member this.IsValidOffset offset =
        let struct(chunki, i) = this.GetIndex offset
        chunki < chunks.Length && i < chunks.[chunki].Length

    member inline this.HasFreeBytes(offset: uint64, length) = this.IsValidOffset(offset + length)

    member this.ReadU1 offset =
        let struct(chunki, i) = this.GetIndex offset
        chunks.[chunki].[i]

    member this.TryAsSpan(offset, length, buffer: outref<Span<byte>>) =
        let struct(chunki, i) = this.GetIndex offset
        let chunk = chunks.[chunki]
        if i + length <= chunk.Length then
            buffer <- Span(chunk, i, buffer.Length)
            true
        else false

    member private this.SlowCopyTo(offset, buffer: Span<byte>) =
        for bufferi = 0 to buffer.Length - 1 do
            buffer.[bufferi] <- this.ReadU1(offset + uint64 bufferi)

    member this.CopyTo(offset, buffer: Span<byte>) =
        let mutable source = Span()
        if this.TryAsSpan(offset, source.Length, &source)
        then source.CopyTo buffer // Copy without having to loop
        else this.SlowCopyTo(offset, buffer)

    member this.TryCopyTo(offset, buffer: Span<byte>) =
        if buffer.IsEmpty
        then true
        elif this.HasFreeBytes(offset, uint64 buffer.Length) then
            this.CopyTo(offset, buffer)
            true
        else false

    member this.ReadBytes(offset, length) =
        let mutable buffer = Span()
        if not(this.TryAsSpan(offset, length, &buffer)) then
            // Cannot span over multiple arrays, so a heap allocation is necessary
            buffer <- Span.heapalloc<byte> length
            this.SlowCopyTo(offset, buffer)
        buffer

    member this.TryParse<'Parser, 'T when 'Parser : struct and 'Parser :> IByteParser<'T>> offset =
        let parser = Unchecked.defaultof<'Parser>
        let buffer = Span.stackalloc<byte> parser.Length
        if this.TryCopyTo(offset, buffer)
        then ValueSome(parser.Parse buffer)
        else ValueNone
