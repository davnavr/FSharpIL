namespace FSharpIL.Reading

open System.Runtime.CompilerServices

/// <summary>Represents an offset into the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
[<IsReadOnly; Struct>]
type ParsedBlob =
    internal { BlobOffset: uint32 }
    static member op_Implicit { BlobOffset = offset } = offset

[<IsReadOnly; Struct>]
type ParsedFieldSig =
    internal { FieldSig: ParsedBlob }

[<RequireQualifiedAccess>]
module ParsedBlob =
    let (|FieldSig|) { FieldSig = blob } = blob

/// <summary>Represents the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
[<Sealed>]
type ParsedBlobStream internal (stream: ParsedMetadataStream) =
    member _.Size = stream.StreamSize
