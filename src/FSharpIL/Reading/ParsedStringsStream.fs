namespace FSharpIL.Reading

open System
open System.Collections.Generic
open System.Text

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata

/// <summary>Represents the <c>#Strings</c> metadata stream, which contains null-terminated UTF-8 strings (II.24.2.3).</summary>
[<Sealed>]
type ParsedStringsStream (stream: ChunkedMemory) =
    let mutable lookup = Dictionary<uint32, string>()
    let mutable buffer = Array.zeroCreate<byte> 32

    new () = ParsedStringsStream ChunkedMemory.empty

    member _.IsEmpty = stream.IsEmpty
    member _.Size = stream.Length

    member _.TryGetString ({ StringOffset = offset' } as offset) =
        if offset' < stream.Length then
            match lookup.TryGetValue offset' with
            | true, existing -> Ok existing
            | false, _ ->
                let mutable i, cont = 0, true

                while cont && uint32 i < stream.Length do
                    let offset'' = uint32 i + offset'
                    if i >= buffer.Length then Array.Resize(&buffer, buffer.Length * 2)
                    match stream.[offset''] with
                    | 0uy -> cont <- false
                    | value ->
                        buffer.[i] <- value
                        i <- i + 1

                let entry = Encoding.UTF8.GetString((ReadOnlySpan buffer).Slice(0, i))
                lookup.[offset'] <- entry
                Ok entry
        else Error(InvalidStringOffset(offset, { StringOffset = stream.Length - 1u }))

[<RequireQualifiedAccess>]
module ParsedStringsStream =
    let tryCreate (stream: ChunkedMemory) =
        if stream.IsEmpty || stream.[stream.Length - 1u] = 0uy
        then Ok(ParsedStringsStream stream)
        else Error MissingStringStreamTerminator
