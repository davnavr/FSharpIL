namespace FSharpIL.Writing

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Utilities.Collections

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Signatures.MetadataSignatures

[<IsReadOnly; Struct>]
type internal BlobEntry =
    { Offset: BlobOffset; DataLength: uint32 }

    member this.TotalLength = (BlobWriter.compressedUnsignedSize this.DataLength) + this.DataLength

type internal IBlobWriter<'Item> = interface
    abstract Write: byref<ChunkedMemoryBuilder> * item: inref<'Item> -> unit
end

type ByteBlobWriter = delegate of byref<ChunkedMemoryBuilder> -> unit

type private DelegateBlobWriter = struct
    interface IBlobWriter<ByteBlobWriter> with member _.Write(wr, writer) = writer.Invoke &wr
end

type private MethodDefSigWriter = struct
    interface IBlobWriter<MethodDefSig> with member _.Write(wr, signature) = BlobWriter.methodDefSig &signature &wr
end

type private CustomAttribWriter = struct
    interface IBlobWriter<CustomAttrib> with member _.Write(wr, attrib) = BlobWriter.customAttrib &attrib &wr
end

/// <summary>Builds the <c>#Blob</c> metadata heap (II.24.2.4).</summary>
[<Sealed>]
type BlobStreamBuilder (capacity: int32) =
    static let empty = Unchecked.defaultof<BlobEntry>
    let entries = RefArrayList<BlobEntry> capacity
    let mutable offset = 1u
    let mutable content = ChunkedMemoryBuilder capacity
    do entries.Add &empty |> ignore

    member _.IsEmpty = entries.Count = 1
    member _.EmptyBlob = empty.Offset

    /// <summary>The length of the <c>#US</c> metadata stream, in bytes.</summary>
    member _.StreamLength = offset

    member private _.AddEntry start =
        let length = content.Length - start
        let entry = { Offset = { BlobOffset = offset }; DataLength = length }

        offset <- offset + entry.TotalLength
        entries.Add &entry |> ignore
        entry.Offset

    // TODO: Add overload that accepts function pointer for adding byte blob when available.
    //member this.Add(writer: 

    member internal this.Add<'Writer, 'Item
        when 'Writer :> IBlobWriter<'Item>
        and 'Writer : struct>
        (item: inref<'Item>)
        =
        let start = content.Length
        Unchecked.defaultof<'Writer>.Write(&content, &item)
        if content.Length = start
        then this.EmptyBlob
        else this.AddEntry start

    member this.Add(writer: ByteBlobWriter) = this.Add<DelegateBlobWriter, _> &writer

    member this.Add(bytes: ReadOnlySpan<byte>) =
        if bytes.Length = 0
        then this.EmptyBlob
        else
            let start = content.Length
            content.Write bytes
            this.AddEntry start

    member inline this.Add(bytes: ReadOnlyMemory<byte>) = this.Add bytes.Span
    member inline this.Add(bytes: byte[]) = this.Add(ReadOnlySpan bytes)
    member inline this.Add(bytes: ImmutableArray<byte>) = this.Add(bytes.AsSpan())

    member this.Add(token: PublicKeyOrToken) =
        { IsPublicKey =
            match token with
            | PublicKeyToken _
            | NoPublicKeyOrToken -> false
            | PublicKey _ -> true
          Token = this.Add(PublicKeyOrToken.toBlock token) }

    member this.Add(signature: inref<_>) = { MethodDefSig = this.Add<MethodDefSigWriter, _> &signature }

    member this.Add(attrib: inref<_>) = { CustomAttrib = this.Add<CustomAttribWriter, _> &attrib }

    interface IStreamBuilder with
        member this.StreamLength = ValueSome this.StreamLength
        member _.StreamName = Magic.StreamNames.blob
        member _.Serialize(wr, _) =
            let mutable offset', content' = 0u, content.ToImmutable() // NOTE: Maybe make an unsafe function that converts it to a ChunkedMemory without copying.
            for i = 0 to entries.Count - 1 do
                let length = entries.[i].DataLength
                BlobWriter.compressedUnsigned length &wr
                wr.Write(content'.Slice(0u, length))
                content' <- content'.Slice length
                offset' <- offset' + length
