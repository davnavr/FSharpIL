namespace FSharpIL.Reading

open System
open System.Runtime.CompilerServices

open FSharpIL

/// <summary>Represents an offset into the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
[<IsReadOnly; Struct>] // TODO: Rename to BlobOffset
type ParsedBlob =
    internal { BlobOffset: uint32 }
    static member op_Implicit { BlobOffset = offset } = offset

type [<IsReadOnly; Struct>] ParsedFieldSig = internal { FieldSig: ParsedBlob }
type [<IsReadOnly; Struct>] ParsedMethodDefSig = internal { MethodDefSig: ParsedBlob }
type [<IsReadOnly; Struct>] ParsedMemberRefSig = internal { MemberRefSig: ParsedBlob }
type [<IsReadOnly; Struct>] ParsedAttributeSig = internal { CustomAttrib: ParsedBlob }
type [<IsReadOnly; Struct>] ParsedStandaloneSig = internal { StandaloneSig: ParsedBlob }
type [<IsReadOnly; Struct>] ParsedPropertySig = internal { PropertySig: ParsedBlob }
type [<IsReadOnly; Struct>] ParsedTypeSpec = internal { TypeSpec: ParsedBlob }
type [<IsReadOnly; Struct>] ParsedMethodInstantiation = internal { MethodSpec: ParsedBlob }
//type [<IsReadOnly; Struct>] TemporarySomethingSig = internal { TemporarySomethingSig: ParsedBlob }

[<RequireQualifiedAccess>]
module ParsedBlob =
    let (|FieldSig|) { FieldSig = blob } = blob

/// <summary>Represents the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
type _ParsedBlobStream =
    internal
        { Chunk: ChunkReader
          BlobsOffset: uint64
          BlobsSize: uint64 }

    member private this.TryRead { BlobOffset = Convert.U8 offset } =
        let mutable size = 0u
        match ParseBlob.tryReadUnsigned (offset + this.BlobsOffset) this.Chunk &size with
        | Ok (Convert.U8 lsize) ->
            let offset', size' = offset + lsize, uint64 size
            if this.Chunk.HasFreeBytes(offset', size')
            then Ok(struct(offset', size'))
            else Error(BlobOutOfBounds(offset, size'))
        | Error err -> Error(InvalidUnsignedCompressedInteger err)

    member this.TryToArray offset =
        match this.TryRead offset with
        | Ok(offset', Convert.I4 size) ->
            // NOTE: This can result in two heap allocations, fix this.
            Ok(this.Chunk.ReadBytes(offset', size).ToArray())
        | Error err -> Error err

[<Sealed>]
type ParsedBlobStream internal (stream: ParsedMetadataStream) =
    member private _.TryRead { BlobOffset = Convert.U8 offset } =
        let mutable size = 0u
        match ParseBlob.tryReadUnsigned (offset + stream.StreamOffset) stream.Chunk &size with
        | Ok (Convert.U8 lsize) ->
            let isize, size' = int32 size, uint64 size
            let offset' = offset + stream.StreamOffset + lsize
            if stream.Chunk.HasFreeBytes(offset', size') then
                if uint32 stream.Buffer.Length < size then
                    stream.Buffer <- Array.zeroCreate isize
                let buffer = Span<byte>(stream.Buffer, 0, isize)
                stream.Chunk.CopyTo(offset', buffer)
                Ok isize
            else Error(BlobOutOfBounds(offset, size'))
        | Error err -> Error(InvalidUnsignedCompressedInteger err)

    // TODO: Add ReadBytes method that throws on error.
    member this.TryReadBytes offset =
        match this.TryRead offset with
        | Ok size -> Ok stream.Buffer.[..size]
        | Error err -> Error err

    member private this.TryReadFieldSig offset =
        match this.TryRead offset with
        | Ok size -> ParseBlob.fieldSig(Span(stream.Buffer, 0, size))
        | Error err -> Error err
    member this.TryReadFieldSig { FieldSig = offset } = this.TryReadFieldSig offset
    member this.TryReadFieldSig { StandaloneSig = offset } = this.TryReadFieldSig offset

    member _.Size = stream.StreamSize
