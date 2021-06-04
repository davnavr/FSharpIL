namespace FSharpIL.Reading

open System
open System.Runtime.CompilerServices

open FSharpIL

[<Obsolete>]
type IByteParser<'Result> =
    abstract Length: int32 // TODO: Make size an unsigned int.
    abstract Parse: Span<byte> -> 'Result // TODO: Replace Span<byte> with Chunks.

[<Obsolete>]
type [<IsReadOnly; Struct>] internal ParseU2 =
    interface IByteParser<uint16> with
        member _.Length = 2
        member _.Parse buffer = Bytes.readU2 0 buffer

[<Obsolete>]
type [<IsReadOnly; Struct>] internal ParseU4 =
    interface IByteParser<uint32> with
        member _.Length = 4
        member _.Parse buffer = Bytes.readU4 0 buffer

[<Obsolete>]
module internal ByteParser =
    let parse<'Parser, 'Result when 'Parser :> IByteParser<'Result>> offset (buffer: Span<byte>) (parser: 'Parser) =
        parser.Parse(buffer.Slice(offset, parser.Length))

    // TODO: Remove this function when/if Parse accepts a Chunks instead.
    // TODO: Rename to tryParseChunk
    let tempParseChunk<'Parser, 'Result when 'Parser :> IByteParser<'Result>>
        offset
        (chunks: inref<ChunkedMemory>)
        (parser: 'Parser)
        =
        let buffer = Span.stackalloc<byte> parser.Length
        if chunks.Slice(offset, uint32 parser.Length).TryCopyTo(0u, buffer)
        then ValueSome(parser.Parse buffer)
        else ValueNone

    let inline tempParseChunkStruct<'Parser, 'Result when 'Parser :> IByteParser<'Result> and 'Parser : struct>
        offset
        (chunks: inref<ChunkedMemory>)
        =
        tempParseChunk<'Parser, 'Result> offset &chunks Unchecked.defaultof<'Parser>
