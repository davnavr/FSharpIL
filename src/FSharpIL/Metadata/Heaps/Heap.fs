namespace FSharpIL.Metadata.Heaps

open System
open System.Collections.Generic
open System.Text

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Metadata
open FSharpIL.Writing

[<RequireQualifiedAccess>]
module internal Heap =
    type Lookup<'T when 'T : equality> (heap: Dictionary<'T, uint32>, byteLength: uint32, indexOf) =
        member _.ByteLength = byteLength
        member _.Count = heap.Count
        member _.ContainsKey item = heap.ContainsKey item
        member _.IndexOf item =
            match indexOf item with
            | Some i -> i
            | None -> heap.Item item

        interface IHeap with
            member this.ByteLength = this.ByteLength

        interface IReadOnlyDictionary<'T, uint32> with
            member this.Count = this.Count
            member this.Item with get item = this.IndexOf item
            member _.Keys = heap.Keys :> IEnumerable<_>
            member _.Values = heap.Values :> IEnumerable<_>
            member this.ContainsKey item = this.ContainsKey item
            member _.GetEnumerator() = heap.GetEnumerator() :> IEnumerator<_>
            member _.GetEnumerator() = heap.GetEnumerator() :> System.Collections.IEnumerator
            member _.TryGetValue(item, index) = heap.TryGetValue(item, &index)

    // TODO: Implement merging of strings that end the same. Ex: "HelloWorld" and "World" end in the same, so indices would be x and x + 5.
    // TODO: Prevent null character from being used in strings.
    /// <summary>Creates the <c>#Strings</c> metadata stream (II.24.2.3).</summary>
    let strings (metadata: CliMetadata) =
        let strings =
            // Estimated number of strings, actual count may be less.
            let capacity =
                1 // Module
                + (2 * metadata.TypeRef.Count)
                + (2 * metadata.TypeDef.Count)
                + metadata.Field.Count
                + metadata.MethodDef.Count
                + metadata.Param.Length

                + metadata.MemberRef.Count

                + metadata.Property.Count

                + if metadata.Assembly.IsSome then 2 else 0
                + (2 * metadata.AssemblyRef.Count)
            Dictionary<_, _> capacity
        let mutable size = 1u

        CliMetadata.iterStrings
            (function
            | ""
            | null -> ()
            | str ->
                if strings.TryAdd(str, size) then
                    size <- size + 1u + (Encoding.UTF8.GetByteCount str |> uint32))
            metadata

        let indexOf =
            function
            | ""
            | null -> Some 0u
            | _ -> None

        Lookup<_>(strings, size, indexOf)

    /// <summary>Creates the <c>#GUID</c> metadata stream (II.24.2.5).</summary>
    let guid (metadata: CliMetadata) =
        let guids = Dictionary<_, _> 1 // Module table is always present

        guids.Add(metadata.Module.Mvid, 1u)

        let indexOf guid =
            if guid = Guid.Empty
            then Some 0u
            else None

        Lookup<_>(guids, uint32 guids.Count * 16u, indexOf)

    let inline private fieldSignature (blob: SerializedBlobHeap) (writer: inref<BlobWriter>) signature =
        if not (blob.FieldSig.ContainsKey signature) then
            writer.FieldSig(blob.Blobs.FieldSig.ItemRef signature)
            blob.CreateIndex(signature, blob.FieldSig)

    /// <summary>Creates the <c>#Blob</c> metadata stream, which contains signatures (II.24.2.4).</summary>
    let blob (metadata: CliMetadata) size =
        let blob = SerializedBlobHeap(metadata.Blobs, size)
        let writer = BlobWriter(metadata, blob.Content)

        // TODO: Verify that the blob's size is automatically reset for each blob

        for field in metadata.Field.Rows do fieldSignature blob &writer field.Signature

        for method in metadata.MethodDef.Rows do
            let i = method.Signature
            if not (blob.MethodDefSig.ContainsKey i) then
                let signature = metadata.Blobs.MethodDefSig.ItemRef i
                let gcount, cconventions =
                    match signature.CallingConventions with
                    | Default -> ValueNone, CallingConvention.Default
                    | VarArg -> ValueNone, CallingConvention.VarArg
                    | Generic cnt -> ValueSome cnt, CallingConvention.Generic

                let mutable flags = cconventions
                if signature.HasThis then flags <- flags ||| CallingConvention.HasThis
                if signature.ExplicitThis then flags <- flags ||| CallingConvention.ExplicitThis
                writer.Writer.WriteU1 flags

                // GenParamCount
                match gcount with
                | ValueSome cnt -> writer.CompressedUnsigned cnt
                | ValueNone -> ()

                writer.CompressedUnsigned signature.Parameters.Length // ParamCount
                writer.RetType signature.ReturnType
                writer.Parameters signature.Parameters
                blob.CreateIndex(i, blob.MethodDefSig)

        for memberRef in metadata.MemberRef.Rows do
            match memberRef.Signature with
            | MemberRefSignature.MethodDefault method ->
                let signature = metadata.Blobs.MethodRefSig.ItemRef method
                writer.Writer.WriteU1 signature.CallingConventions
                writer.CompressedUnsigned signature.Parameters.Length // ParamCount
                writer.RetType signature.ReturnType
                writer.Parameters signature.Parameters
            | MemberRefSignature.MethodGeneric method ->
                let signature = metadata.Blobs.MethodRefSig.ItemRef method
                writer.Writer.WriteU1 signature.CallingConventions
                writer.CompressedUnsigned signature.GenParamCount
                writer.CompressedUnsigned signature.Parameters.Length // ParamCount
                writer.RetType signature.ReturnType
                writer.Parameters signature.Parameters
            | MemberRefSignature.MethodVarArg method ->
                let signature = metadata.Blobs.MethodRefSig.ItemRef method
                writer.Writer.WriteU1 signature.CallingConventions
                writer.CompressedUnsigned signature.ParamCount
                writer.RetType signature.ReturnType
                writer.Parameters signature.Parameters
                if not signature.VarArgParameters.IsEmpty then
                    writer.Writer.WriteU1 ElementType.Sentinel
                    writer.Parameters signature.VarArgParameters
            | MemberRefSignature.Field field ->
                fieldSignature blob &writer field
            failwith "TODO: Write MethodRef"

        for row in metadata.Constant.Rows do
            let i = row.Value
            if not (blob.Constant.ContainsKey i) then
                match i with
                | ConstantBlob.Integer i' ->
                    let value = metadata.Blobs.Constant.ItemRef i'
                    match value.Tag with
                    | IntegerType.Bool
                    | IntegerType.I1
                    | IntegerType.U1 -> writer.Writer.WriteU1 value.U1
                    | IntegerType.Char
                    | IntegerType.I2
                    | IntegerType.U2 -> writer.Writer.WriteU2 value.U2
                    | IntegerType.I4
                    | IntegerType.U4 -> writer.Writer.WriteU4 value.U4
                    | IntegerType.I8
                    | IntegerType.U8 -> writer.Writer.WriteU4 value.U4
                    | _ -> invalidOp "Cannot write integer constant blob for unknown integer type"
                | ConstantBlob.String i' -> failwith "TODO: String constant values are not supported at this time"
                | ConstantBlob.Null -> writer.Writer.WriteU4 0u

                blob.CreateIndex(i, blob.Constant)

        for { Value = value } in metadata.CustomAttribute do
            match value with
            | ValueSome i when not (blob.CustomAttribute.ContainsKey i) ->
                let signature = metadata.Blobs.CustomAttribue.ItemRef i
                writer.Writer.WriteU2 1us // Prolog
                for arg in signature.FixedArg do
                    writer.FixedArg arg
                writer.Writer.WriteU2 signature.NamedArg.Length // NumNamed
                for arg in signature.NamedArg do
                    failwithf "TODO: Implement writing of named arguments for custom attributes"
                blob.CreateIndex(i, blob.CustomAttribute)
            | _ -> ()

        for row in metadata.Property.Rows do
            let i = row.Type
            if not (blob.PropertySig.ContainsKey i) then
                let signature = metadata.Blobs.PropertySig.ItemRef i
                let property = if signature.HasThis then 0x28uy else 0x8uy
                writer.Writer.WriteU1 property
                writer.CompressedUnsigned signature.Parameters.Length // ParamCount
                writer.CustomMod signature.CustomMod
                writer.EncodedType signature.Type
                writer.Parameters signature.Parameters
                blob.CreateIndex(i, blob.PropertySig)

        for typeSpec in metadata.TypeSpec.Rows do
            let i = typeSpec.Signature
            if not (blob.TypeSpec.ContainsKey i) then
                let signature = metadata.Blobs.TypeSpec.ItemRef i

                match signature with
                | TypeSpec.GenericInst inst -> writer.GenericInst inst
                // TODO: Factor out common code shared between writing of TypeSpec and encoded Type.
                | TypeSpec.MVar num ->
                    writer.Writer.WriteU1 ElementType.MVar
                    writer.CompressedUnsigned num
                | TypeSpec.Var num ->
                    writer.Writer.WriteU1 ElementType.Var
                    writer.CompressedUnsigned num

                blob.CreateIndex(i, blob.TypeSpec)

        for methodSpec in metadata.MethodSpec.Rows do
            let i = methodSpec.Instantiation
            if not (blob.MethodSpec.ContainsKey i) then
                let inst = metadata.Blobs.MethodSpec.ItemRef i
                writer.Writer.WriteU1 0xAuy // GENERICINST
                writer.CompressedUnsigned inst.Count
                for gparam in inst.GenericArguments do writer.EncodedType gparam
                blob.CreateIndex(i, blob.MethodSpec)

        for { PublicKeyOrToken = token } in metadata.AssemblyRef.Rows do
            match token.AsByteBlob() with
            | ValueSome i ->
                let token = metadata.Blobs.MiscBytes.ItemRef i
                writer.Writer.WriteBytes token
                blob.CreateIndex(i, blob.MiscBytes)
            | ValueNone -> ()

        for locals in metadata.StandAloneSig.LocalVariables do
            if not (blob.LocalVarSig.ContainsKey locals) then
                let signature = metadata.Blobs.LocalVarSig.ItemRef locals
                writer.Writer.WriteU1 0x7uy // LOCAL_SIG
                writer.CompressedUnsigned signature.Length // Count
                for local in signature do
                    match local with
                    | LocalVariable.TypedByRef -> writer.Writer.WriteU1 ElementType.TypedByRef
                    | _ ->
                        writer.CustomMod local.CustomMod
                        for constr in local.Constraints do
                            match constr with
                            | LocalVariableConstraint.Pinned -> writer.Writer.WriteU1 ElementType.Pinned
                        if local.Tag = LocalVariableTag.ByRef then
                            writer.Writer.WriteU1 ElementType.ByRef
                        writer.EncodedType local.LocalType
                blob.CreateIndex(locals, blob.LocalVarSig)

        for { File.HashValue = i } in metadata.File.Rows do
            if not (blob.MiscBytes.ContainsKey i) then
                let hash = metadata.Blobs.MiscBytes.ItemRef i
                writer.Writer.WriteBytes hash
                blob.CreateIndex(i, blob.MiscBytes)

        blob

    let writeStrings (count: int32) (metadata: CliMetadata) (writer: ChunkWriter) =
        let lookup = HashSet<string> count
        writer.WriteU1 0uy
        CliMetadata.iterStrings
            (function
            | ""
            | null -> ()
            | str ->
                if lookup.Add str then
                    Encoding.UTF8.GetBytes str |> writer.WriteBytes
                    writer.WriteU1 0uy)
            metadata

    let writeGuid (metadata: CliMetadata) (writer: ChunkWriter) =
        metadata.Module.Mvid.ToByteArray() |> writer.WriteBytes

    let writeBlob (blobs: SerializedBlobHeap) metadata (writer: ChunkWriter) =
        writer.WriteU1 0uy

        let mutable chunk, i = blobs.Content.Chunk.List.First, 0
        let writeAll (dict: Dictionary<_, BlobIndex>) =
            let writer' = BlobWriter(metadata, writer)
            for KeyValue(_, index) in dict do
                writer'.CompressedUnsigned index.Size
                let struct(chunk', i') = writer.WriteBytes(chunk, i, index.Size)
                chunk <- chunk'
                i <- i'

        writeAll blobs.FieldSig
        writeAll blobs.MethodDefSig
        writeAll blobs.MethodRefSig
        writeAll blobs.Constant
        writeAll blobs.CustomAttribute
        writeAll blobs.PropertySig
        writeAll blobs.TypeSpec
        writeAll blobs.MethodSpec
        writeAll blobs.LocalVarSig
        writeAll blobs.MiscBytes

    let writeUS (us: UserStringHeap) metadata (writer: ChunkWriter) =
        let writer' = BlobWriter(metadata, writer)
        writer.WriteU1 0uy
        for str in us do
            let { BlobIndex.Size = size } = us.IndexOf str
            writer'.CompressedUnsigned size
            Encoding.Unicode.GetBytes str |> writer.WriteBytes
            writer.WriteU1 0uy // TODO: Determine value of terminal byte.

type internal Heap<'T when 'T : equality> = Heap.Lookup<'T>
