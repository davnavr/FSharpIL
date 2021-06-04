namespace FSharpIL

type internal IByteReader<'Result when 'Result : struct> = abstract Read: ChunkedMemory -> 'Result

[<RequireQualifiedAccess>]
module internal ByteReader =
    let read offset (data: ChunkedMemory) (parser: #IByteReader<'Result>) =
        parser.Read(data.Slice(offset, uint32 sizeof<'Result>))
    let inline quickRead<'Reader, 'Result when 'Reader :> IByteReader<'Result> and 'Reader : struct> (data: ChunkedMemory) =
        Unchecked.defaultof<'Reader>.Read data
