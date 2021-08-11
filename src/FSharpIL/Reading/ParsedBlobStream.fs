namespace FSharpIL.Reading

open FSharpIL
open FSharpIL.Metadata

/// <summary>Represents the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
[<Sealed>]
type ParsedBlobStream internal (stream: LengthEncodedStream) =
    // TODO: Have lookups for various signature types.
    //let 

    new (stream) = ParsedBlobStream(LengthEncodedStream stream)
    new () = ParsedBlobStream ChunkedMemory.empty

    member _.Size = stream.contents.Length

    /// <summary>Returns the contents of the blob at the specified offset.</summary>
    member _.TryGetBytes { BlobOffset = offset } = stream.TryReadBytes offset
