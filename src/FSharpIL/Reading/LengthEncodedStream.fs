namespace FSharpIL.Reading

open FSharpIL

/// Represents a metadata stream containing blobs, whose lengths are included before the corresponding blob's content (II.24.2.4).
[<System.Runtime.CompilerServices.IsReadOnly>]
type internal LengthEncodedStream = struct
    val contents: ChunkedMemory
    new (contents) = { contents = contents }
    member this.TryReadBytes offset =
        match this.contents.TrySlice offset with
        | true, chunk' ->
            let mutable chunk' = chunk'
            match ParseBlob.compressedUnsigned &chunk' with
            | Ok(_, size: uint32) ->
                match chunk'.TrySlice(0u, size) with
                | true, blob -> Ok blob
                | false, _ -> Error(BlobOutsideOfHeap(offset, size))
            | Error err -> Error err
        | false, _ -> Error(InvalidBlobOffset(offset, this.contents.Length - 1u))
end
