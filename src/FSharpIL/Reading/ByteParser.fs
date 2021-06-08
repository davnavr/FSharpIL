namespace FSharpIL.Reading

open FSharpIL

type IByteParser<'T when 'T : struct> = interface
    abstract Parse : buffer: ChunkedMemory -> 'T
    abstract Length : uint32
end

[<RequireQualifiedAccess>]
module ByteParser =
    let inline length (parser: #IByteParser<_>) = parser.Length
    let parse offset (chunks: inref<ChunkedMemory>) (parser: #IByteParser<'Result>) =
        parser.Parse(chunks.Slice(offset, parser.Length))
