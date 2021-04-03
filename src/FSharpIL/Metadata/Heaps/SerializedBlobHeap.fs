namespace FSharpIL.Metadata.Heaps

open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Writing

type internal BlobIndexLookup<'Item> = Dictionary<Blob<'Item>, BlobIndex>

[<Sealed>]
type internal SerializedBlobHeap (blobs: BlobHeap, size) =
    let writer =
        let content = LinkedList<byte[]>()
        ChunkWriter(content.AddFirst(Array.zeroCreate size))
    let mutable offset, size = 1u, writer.Size

    member val FieldSig = BlobIndexLookup<FieldSignature> blobs.FieldSig.Count
    member val MethodDefSig = BlobIndexLookup<MethodDefSignature> blobs.MethodDefSig.Count
    member val MethodRefSig = Dictionary<MethodRefSignature, BlobIndex> blobs.MethodRefSig.Count
    member val Constant = Dictionary<ConstantBlob, BlobIndex> blobs.Constant.Count
    member val CustomAttribute = BlobIndexLookup<CustomAttributeSignature> blobs.CustomAttribue.Count

    member val PropertySig = BlobIndexLookup<PropertySignature> blobs.PropertySig.Count

    member val TypeSpec = Dictionary<TypeSpecBlob, BlobIndex> blobs.TypeSpec.Count

    member val MethodSpec = Dictionary<MethodSpecBlob, BlobIndex> blobs.MethodSpec.Count

    member val LocalVarSig = BlobIndexLookup<MethodLocalVariables> blobs.LocalVarSig.Count
    member val MiscBytes = BlobIndexLookup<ImmutableArray<byte>> blobs.MiscBytes.Count

    member _.Blobs = blobs
    member _.Content = writer
    member this.SignatureCount =
        this.FieldSig.Count
        + this.MethodDefSig.Count
        + this.MethodRefSig.Count
        + this.Constant.Count
        + this.CustomAttribute.Count

        + this.PropertySig.Count

        + this.TypeSpec.Count

        + this.MethodSpec.Count

        + this.LocalVarSig.Count
        + this.MiscBytes.Count

    member _.CreateIndex(blob: 'Item, lookup: Dictionary<'Item, BlobIndex>) =
        let size' = writer.Size - size
        size <- writer.Size
        lookup.Item <- blob, { BlobIndex.Index = offset; BlobIndex.Size = size' }
        offset <- offset + size' + BlobSize.ofUnsigned size'

    member private this.WriteRawIndex(i, writer: ChunkWriter) =
        match this with
        | LargeIndex -> writer.WriteU4 i
        | SmallIndex -> writer.WriteU2 i

    member inline private this.WriteRawIndex({ BlobIndex.Index = i }, writer: ChunkWriter) = this.WriteRawIndex(i, writer)

    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.FieldSig.Item signature, writer)
    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.MethodDefSig.Item signature, writer)
    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.MethodRefSig.Item signature, writer)
    member this.WriteIndex(value, writer) = this.WriteRawIndex(this.Constant.Item value, writer)
    member this.WriteIndex(signature, writer) =
        match signature with
        | ValueSome signature' -> this.WriteRawIndex(this.CustomAttribute.Item signature', writer)
        | ValueNone -> this.WriteRawIndex(0u, writer)

    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.PropertySig.Item signature, writer)

    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.TypeSpec.Item signature, writer)

    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.MethodSpec.Item signature, writer)

    member this.WriteIndex(token, writer) = this.WriteRawIndex(this.LocalVarSig.Item token, writer)
    member this.WriteIndex(bytes, writer) = this.WriteRawIndex(this.MiscBytes.Item bytes, writer)

    member this.WriteEmpty writer = this.WriteRawIndex(0u, writer)

    interface IHeap with member _.ByteLength = writer.Size
