namespace FSharpIL.Reading

open FSharpIL
open FSharpIL.Metadata

/// <summary>Represents the <c>#US</c> metadata heap, which contains UTF-16 strings (II.24.2.4).</summary>
type ParsedUserStringStream internal (stream: LengthEncodedStream) =
    let lookup = System.Collections.Generic.Dictionary<uint32, string>()

    new (stream) = new ParsedUserStringStream(LengthEncodedStream stream)
    new () = ParsedUserStringStream ChunkedMemory.empty

    member _.Size = stream.contents.Length

    member _.TryGetString { UserStringOffset = offset } =
        match lookup.TryGetValue offset with
        | true, existing -> Ok existing
        | false, _ ->
            stream.TryReadBytes offset
            |> Utilities.Fail.noImpl "TODO: Read the blob"
