﻿namespace FSharpIL.Reading

open System
open System.Runtime.CompilerServices

open FSharpIL

/// <summary>Represents an offset into the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
[<IsReadOnly; Struct>]
type ParsedBlob =
    internal { BlobOffset: uint32 }
    static member op_Implicit { BlobOffset = offset } = offset

[<IsReadOnly; Struct>]
type ParsedFieldSig = internal { FieldSig: ParsedBlob }

[<IsReadOnly; Struct>]
type ParsedMethodDefSig = internal { MethodDefSig: ParsedBlob }

[<RequireQualifiedAccess>]
module ParsedBlob =
    let (|FieldSig|) { FieldSig = blob } = blob

/// <summary>Represents the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
[<Sealed>]
type ParsedBlobStream internal (stream: ParsedMetadataStream) =
    member private _.TryRead { BlobOffset = Convert.U8 offset } =
        let mutable size = 0u
        match ParsedMetadataStream.tryReadUnsigned offset stream &size with
        | Ok (Convert.U8 lsize) ->
            let isize, size' = int32 size, uint64 size
            let offset' = offset + lsize
            if stream.Chunk.HasFreeBytes(offset', size') then
                if uint32 stream.Buffer.Length < size then
                    stream.Buffer <- Array.zeroCreate isize
                let buffer = Span<byte>(stream.Buffer, 0, isize)
                stream.Chunk.ReadBytes(offset', buffer)
                Ok isize
            else Error(BlobOutOfBounds(offset, size'))
        | Error err -> Error(InvalidUnsignedCompressedInteger err)
    member this.TryReadBytes offset =
        match this.TryRead offset with
        | Ok size -> Ok stream.Buffer.[..size]
        | Error err -> Error err
    member _.Size = stream.StreamSize
