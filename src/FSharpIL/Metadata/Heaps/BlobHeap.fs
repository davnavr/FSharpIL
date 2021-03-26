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

// TODO: Figure out how to allow easy lookup of blob indices while avoiding copying of large structs.
/// <summary>Represents the <c>#Blob</c> metadata stream (II.24.2.4).</summary>
[<ReferenceEquality; NoComparison>]
type internal BlobHeap =
    { Field: Dictionary<FieldSignature , BlobIndex>
      MethodDef: Dictionary<MethodDefSignature, BlobIndex>
      MemberRef: Dictionary<MemberRefRow, BlobIndex>
      Constant: Dictionary<ConstantValue, BlobIndex>
      CustomAttribute: Dictionary<CustomAttributeSignature, BlobIndex>

      Property: Dictionary<PropertySignature, BlobIndex>

      TypeSpec: Dictionary<TypeSpec, BlobIndex>

      MethodSpec: Dictionary<MethodSpec, BlobIndex>

      PublicKeyTokens: Dictionary<PublicKeyOrToken, BlobIndex>
      // TODO: See if having key of local variable signature dictionary be an ImmutableArray is bad for performance.
      LocalVariables: Dictionary<MethodLocalVariables, BlobIndex>
      ByteBlobs: Dictionary<byte[], BlobIndex>
      Content: ChunkWriter }

    member this.SignatureCount =
        this.Field.Count
        + this.MethodDef.Count
        + this.MemberRef.Count
        + this.Constant.Count
        + this.CustomAttribute.Count

        + this.Property.Count

        + this.TypeSpec.Count

        + this.MethodSpec.Count

         + this.PublicKeyTokens.Count
         + this.LocalVariables.Count
         + this.ByteBlobs.Count

    member private this.WriteRawIndex(i, writer: ChunkWriter) =
        match this with
        | LargeIndex -> writer.WriteU4 i
        | SmallIndex -> writer.WriteU2 i

    member inline private this.WriteRawIndex({ BlobIndex.Index = i }, writer: ChunkWriter) = this.WriteRawIndex(i, writer)

    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.Field.Item signature, writer)
    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.MethodDef.Item signature, writer)
    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.MemberRef.Item signature, writer)
    member this.WriteIndex(value, writer) = this.WriteRawIndex(this.Constant.Item value, writer)
    member this.WriteIndex(signature, writer) =
        match signature with
        | Some signature' -> this.WriteRawIndex(this.CustomAttribute.Item signature', writer)
        | None -> this.WriteRawIndex(0u, writer)

    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.Property.Item signature, writer)

    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.TypeSpec.Item signature, writer)

    member this.WriteIndex(signature, writer) = this.WriteRawIndex(this.MethodSpec.Item signature, writer)

    member this.WriteIndex(token, writer) = this.WriteRawIndex(this.PublicKeyTokens.Item token, writer)
    member this.WriteIndex(token, writer) = this.WriteRawIndex(this.LocalVariables.Item token, writer)
    member this.WriteIndex(bytes, writer) = this.WriteRawIndex(this.ByteBlobs.Item bytes, writer)

    member this.WriteEmpty writer = this.WriteRawIndex(0u, writer)

    interface IHeap with member this.ByteLength = this.Content.Size
