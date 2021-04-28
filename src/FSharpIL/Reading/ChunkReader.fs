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

    member this.ValidOffset offset =
        let struct(chunki, i) = this.GetIndex offset
        chunki < chunks.Length && i < chunks.[chunki].Length

    member inline this.HasFreeBytes(offset: uint64, length) = this.ValidOffset(offset + length)

    member this.ReadU1 offset =
        let struct(chunki, i) = this.GetIndex offset
        chunks.[chunki].[i]

    member this.ReadBytes(offset, buffer: Span<byte>) =
        if buffer.IsEmpty
        then true
        elif this.HasFreeBytes(offset, uint64 buffer.Length) then
            let struct(chunki, i) = this.GetIndex offset
            let chunk = chunks.[chunki]
            if i + buffer.Length <= chunk.Length then
                // Copy without having to loop
                Span(chunk, i, buffer.Length).CopyTo buffer
            else
                for bufferi = 0 to buffer.Length - 1 do
                    buffer.[bufferi] <- this.ReadU1(offset + uint64 bufferi)
            true
        else false

    member this.Parse<'Parser, 'T when 'Parser : struct and 'Parser :> IByteParser<'T>> offset =
        let parser = Unchecked.defaultof<'Parser>
        let buffer = Span.stackalloc<byte> parser.Length
        if this.ReadBytes(offset, buffer)
        then ValueSome(parser.Parse buffer)
        else ValueNone

    [<Obsolete>]
    member this.ReadU2 offset =
        let buffer = Span.stackalloc<byte> 2
        if this.ReadBytes(offset, buffer)
        then ValueSome(Bytes.readU2 0 buffer)
        else ValueNone

    [<Obsolete>]
    member this.ReadU4 offset =
        let buffer = Span.stackalloc<byte> 4
        if this.ReadBytes(offset, buffer)
        then ValueSome(Bytes.readU4 0 buffer)
        else ValueNone
