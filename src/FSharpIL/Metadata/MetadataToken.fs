/// Provides functions for writing metadata tokens.
[<RequireQualifiedAccess>]
module internal FSharpIL.Metadata.MetadataToken

open FSharpIL.Writing

open FSharpIL.Metadata.Heaps

/// <summary>
/// Writes a metadata token, which is used as an argument to some CIL instructions (III.1.9).
/// </summary>
/// <exception cref="T:System.ArgumentException">The <paramref name="index"/> cannot be represented in 3 bytes.</exception>
let write index table (writer: ChunkWriter) =
    if index > 0xFF_FF_FFu then
        invalidArg
            (nameof index)
            "The row or offset pointed to by a metadata token must be able to fit in 3 bytes."
    index &&& 0xFFu |> writer.WriteU1
    (index >>> 8) &&& 0xFFu |> writer.WriteU1
    (index >>> 16) &&& 0xFFu |> writer.WriteU1
    writer.WriteU1 table

let callee (metadata: CliMetadata) =
    function
    | Callee.MethodRef { MemberRefHandle = method } ->
        write (metadata.MemberRef.IndexOf method) 0xAuy

let userString str (us: UserStringHeap) =
    us.Add str
    let { BlobIndex.Index = i } = us.IndexOf str
    write i 0x70uy
