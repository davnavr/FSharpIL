namespace FSharpIL.Reading

open System

open FSharpIL

type IByteParser<'Result> =
    abstract Length: int32
    abstract Parse: Span<byte> -> 'Result

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
