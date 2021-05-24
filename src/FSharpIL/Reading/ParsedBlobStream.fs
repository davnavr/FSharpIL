namespace FSharpIL.Reading

open System
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Utilities

/// <summary>Represents an offset into the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
[<IsReadOnly; Struct>] // TODO: Rename to BlobOffset
type ParsedBlob =
    internal { BlobOffset: uint32 }
    static member op_Implicit { BlobOffset = offset } = offset

// TODO: Rename other types ending in Sig to end in Offset instead.
type [<IsReadOnly; Struct>] FieldSigOffset = internal { FieldSig: ParsedBlob }
type [<IsReadOnly; Struct>] MethodDefSigOffset = internal { MethodDefSig: ParsedBlob }
type [<IsReadOnly; Struct>] ParsedMemberRefSig = internal { MemberRefSig: ParsedBlob }
type [<IsReadOnly; Struct>] CustomAttribOffset = internal { CustomAttrib: ParsedBlob }
type [<IsReadOnly; Struct>] ParsedStandaloneSig = internal { StandaloneSig: ParsedBlob }
type [<IsReadOnly; Struct>] PropertySigOffset = internal { PropertySig: ParsedBlob }
type [<IsReadOnly; Struct>] TypeSpecOffset = internal { TypeSpec: ParsedBlob }
type [<IsReadOnly; Struct>] ParsedMethodInstantiation = internal { MethodSpec: ParsedBlob }
//type [<IsReadOnly; Struct>] TemporarySomethingSig = internal { TemporarySomethingSig: ParsedBlob }

[<RequireQualifiedAccess>]
module ParsedBlob =
    let (|FieldSig|) { FieldSig = blob } = blob

/// <summary>Represents the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
[<Sealed>]
type ParsedBlobStream internal (chunk: ChunkedMemory) =
    member _.Size = chunk.Length

    member _.TryRead { BlobOffset = offset } =
        match chunk.TrySlice offset with
        | true, chunk' ->
            let mutable chunk' = chunk'
            match ParseBlob.compressedUnsigned &chunk' with
            | Ok(Convert.U4 lsize, size) ->
                let offset' = offset + lsize
                match chunk.TrySlice(offset', size) with
                | true, blob -> Ok blob
                | false, _ -> Error(BlobOutsideOfHeap(offset, size))
            | Error err -> Error err
        | false, _ -> Error(InvalidBlobOffset(offset, chunk.Length - 1u))

    /// <summary>Returns the contents of the blob at the specified <paramref name="offset"/>.</summary>
    [<Obsolete("Simple use the raw chunk instead of having to allocate an array for every blob.")>]
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

    member this.TryReadMethodDefSig { MethodDefSig = offset } =
        match this.TryRead offset with
        | Ok signature -> ParseBlob.methodDefSig &signature
        | Error err -> Error err

    member this.TryReadPropertySig { PropertySig = offset } =
        match this.TryRead offset with
        | Ok signature -> ParseBlob.propertySig &signature
        | Error err -> Error err

    member this.TryReadTypeSpec { TypeSpec = offset } =
        match this.TryRead offset with
        | Ok signature -> ParseBlob.typeSpec &signature
        | Error err -> Error err
