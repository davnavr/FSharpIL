namespace FSharpIL.Reading

open System
open System.Text

open FSharpIL.Metadata

/// <summary>Reads strings from the <c>#Strings</c> metadata heap (II.24.2.3).</summary>
// TODO: Might be able to make reading more optimized if size: uint64 field is provided, since we can check how many bytes before end of stream.
[<Sealed>]
type StringReader internal (chunk: ChunkReader, offset: uint64) =
    let mutable buffer = Array.zeroCreate<byte> 32;

    member private _.AppendByte(i, value) =
        if i >= buffer.Length then
            let buffer' = Array.zeroCreate<byte>(buffer.Length * 2)
            Span(buffer).CopyTo(Span buffer')
            buffer <- buffer'
        buffer.[i] <- value

    member private _.GetFromBuffer length = Encoding.UTF8.GetString(ReadOnlySpan(buffer, 0, length))

    member this.TryGetString(index: uint64) =
        let rec inner i =
            let offset' = offset + index + uint64 i
            if chunk.ValidOffset offset' then
                match chunk.ReadU1 offset' with
                | 0uy -> Ok(this.GetFromBuffer i)
                | value ->
                    this.AppendByte(i, value)
                    inner (i + 1)
            else Error(this.GetFromBuffer(i + 1))
        inner 0

    member this.TryGetString(index: RawIndex<string>) = this.TryGetString(uint64 index.Value)

[<RequireQualifiedAccess>]
module StringReader =
    let internal ofHeader (chunk: ChunkReader) header =
        match header with
        | ValueSome { ParsedStreamHeader.Offset = offset; Size = size } when chunk.HasFreeBytes(uint64 offset, uint64 size) ->
            ValueSome(StringReader(chunk, uint64 offset))
        | _ -> ValueNone
