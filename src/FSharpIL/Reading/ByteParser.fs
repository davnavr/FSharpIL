namespace FSharpIL.Reading

open System

open FSharpIL

type internal IByteParser<'Result> =
    abstract Length: int32
    abstract Parse: Span<byte> -> 'Result

[<RequireQualifiedAccess>]
module internal ByteParser =
    type [<Struct>] U2 =
        interface IByteParser<uint16> with
            member _.Length = 2
            member _.Parse buffer = Bytes.readU2 0 buffer

    type [<Struct>] U4 =
        interface IByteParser<uint32> with
            member _.Length = 4
            member _.Parse buffer = Bytes.readU4 0 buffer
