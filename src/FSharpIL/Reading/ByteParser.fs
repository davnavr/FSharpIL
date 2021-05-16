namespace FSharpIL.Reading

open System

open FSharpIL

type IByteParser<'Result> =
    abstract Length: int32 // TODO: Make size an unsigned int.
    abstract Parse: Span<byte> -> 'Result // TODO: Replace Span<byte> with Chunks.

type [<Struct>] internal ParseU2 =
    interface IByteParser<uint16> with
        member _.Length = 2
        member _.Parse buffer = Bytes.readU2 0 buffer

type [<Struct>] internal ParseU4 =
    interface IByteParser<uint32> with
        member _.Length = 4
        member _.Parse buffer = Bytes.readU4 0 buffer

module internal ByteParser =
    // TODO: This might replace Bytes.readU* functions.
    let parse<'Parser, 'Result when 'Parser :> IByteParser<'Result>> offset (buffer: Span<byte>) (parser: 'Parser) =
        parser.Parse(buffer.Slice(offset, parser.Length))

    // TODO: Remove this function when/if Parse accepts a Chunks instead.
    let tempParseChunk<'Parser, 'Result when 'Parser :> IByteParser<'Result>> offset (chunks: Chunks) (parser: 'Parser) =
        let buffer = Span.stackalloc<byte> parser.Length
        if Chunks.tryCopyToSpan (Chunks.slice offset (uint32 parser.Length) chunks) 0u buffer then
            ValueSome(parser.Parse buffer)
        else ValueNone

    let inline tempParseChunkStruct<'Parser, 'Result when 'Parser :> IByteParser<'Result> and 'Parser : struct> offset chunks =
        tempParseChunk<'Parser, 'Result> offset chunks Unchecked.defaultof<'Parser>
