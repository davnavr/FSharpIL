namespace FSharpIL.Reading

open FSharpIL
open FSharpIL.Utilities

[<System.Runtime.CompilerServices.IsReadOnly>]
type internal LengthEncodedStream = struct
    val contents: ChunkedMemory
    new (contents) = { contents = contents }
    member this.TryReadBytes offset =
        match this.contents.TrySlice offset with
        | true, chunk' ->
            let mutable chunk' = chunk'
            match ParseBlob.compressedUnsigned &chunk' with
            | Ok(Convert.U4 lsize, size: uint32) ->
                let offset' = offset + lsize
                match chunk'.TrySlice(offset', size) with
                | true, blob -> Ok blob
                | false, _ -> Error(BlobOutsideOfHeap(offset, size))
            | Error err -> Error err
        | false, _ -> Error(InvalidBlobOffset(offset, this.contents.Length - 1u))
end
