namespace FSharpIL.Reading

/// <summary>Represents an offset into the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ParsedBlob =
    internal { BlobOffset: uint32 }
    static member op_Implicit { BlobOffset = offset } = offset

/// <summary>Represents the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
[<Sealed>]
type ParsedBlobStream internal (stream: ParsedMetadataStream) =
    member _.Size = stream.StreamSize
