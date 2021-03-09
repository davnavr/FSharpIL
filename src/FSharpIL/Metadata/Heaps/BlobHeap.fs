namespace FSharpIL.Metadata.Heaps

open System.Collections.Generic

open FSharpIL.Metadata

open FSharpIL.Writing

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type internal BlobIndex =
    { /// <summary>An index that points to this blob in the <c>#Blob</c> heap.</summary>
      Index: uint32
      /// The size of the contents of the blob pointed to by this index.
      Size: uint32 }

/// <summary>Represents the <c>#Blob</c> metadata stream (II.24.2.4).</summary>
[<ReferenceEquality; NoComparison>]
type internal BlobHeap =
    { Field: Dictionary<FieldSignature , BlobIndex>
      MethodDef: Dictionary<MethodDefSignature, BlobIndex>
      MemberRef: Dictionary<MemberRefRow, BlobIndex>

      CustomAttribute: Dictionary<CustomAttributeSignature, BlobIndex>

      TypeSpec: Dictionary<TypeSpec, BlobIndex>

      MethodSpec: Dictionary<MethodSpec, BlobIndex>

      PublicKeyTokens: Dictionary<PublicKeyOrToken, BlobIndex>
      ByteBlobs: Dictionary<byte[], BlobIndex>
      Content: ChunkWriter }

    // TODO: Turn these Blob methods into functions.
    member this.SignatureCount =
        this.Field.Count
        + this.MethodDef.Count
        + this.MemberRef.Count

        + this.CustomAttribute.Count

        + this.TypeSpec.Count

        + this.MethodSpec.Count

         + this.PublicKeyTokens.Count
         + this.ByteBlobs.Count

    member private this.WriteRawIndex(i, writer: ChunkWriter) =
        match this with
        | LargeIndex -> writer.WriteU4 i
        | SmallIndex -> writer.WriteU2 i

    member inline private this.WriteRawIndex({ BlobIndex.Index = i }, writer: ChunkWriter) = this.WriteRawIndex(i, writer)

    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.Field.Item signature, writer)
    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.MethodDef.Item signature, writer)
    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.MemberRef.Item signature, writer)
    member this.WriteIndex(signature, writer) =
        match signature with
        | Some signature' -> this.WriteRawIndex(this.CustomAttribute.Item signature', writer)
        | None -> ()

    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.TypeSpec.Item signature, writer)

    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.MethodSpec.Item signature, writer)

    member this.WriteIndex(token, writer) = this.WriteRawIndex(this.PublicKeyTokens.Item token, writer)
    member this.WriteIndex(bytes, writer) = this.WriteRawIndex(this.ByteBlobs.Item bytes, writer)

    member this.WriteEmpty writer = this.WriteRawIndex(0u, writer)

    interface IHeap with member this.ByteLength = this.Content.Size
