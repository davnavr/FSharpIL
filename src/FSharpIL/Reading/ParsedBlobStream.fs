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
type ParsedBlobStream internal (chunk: ChunkedMemory) =
    member _.Size = chunk.Length

    member private _.TryRead { BlobOffset = offset } =
        let mutable size = 0u
        match ParseBlob.tryReadUnsigned offset &chunk &size with
        | Ok (Convert.U4 lsize) ->
            let offset', size' = offset + lsize, size
            match chunk.TrySlice(offset', size) with
            | true, blob -> Ok blob
            | false, _ -> Error(BlobOutOfBounds(offset, size'))
        | Error err -> Error(InvalidUnsignedCompressedInteger err)

    /// <summary>Returns the contents of the blob at the specified <paramref name="offset"/>.</summary>
    member this.TryReadBytes offset =
        match this.TryRead offset with
        | Ok blob -> Ok(blob.ToImmutableArray())
        | Error err -> Error err

    member private this.TryReadFieldSig offset =
        match this.TryRead offset with
        | Ok signature -> ParseBlob.fieldSig &signature
        | Error err -> Error err
    member this.TryReadFieldSig { FieldSig = offset } = this.TryReadFieldSig offset
    member this.TryReadFieldSig { StandaloneSig = offset } = this.TryReadFieldSig offset
