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

    /// <summary>Creates the <c>#Blob</c> metadata stream, which contains signatures (II.24.2.4).</summary>
    let blob (metadata: CliMetadata) size =
        let mutable offset = 1u
        let blob =
            { Field = Dictionary<_, _> metadata.Field.Count
              MethodDef = Dictionary<_, _> metadata.MethodDef.Count
              MemberRef = Dictionary<_, _> metadata.MemberRef.Count

              CustomAttribute = Dictionary<_, _> metadata.CustomAttribute.Length

              Property = Dictionary<_, _> metadata.Property.Count

              TypeSpec = Dictionary<_, _> metadata.TypeSpec.Count

              MethodSpec = Dictionary<_, _> metadata.MethodSpec.Count

              PublicKeyTokens = Dictionary<_, _> metadata.AssemblyRef.Count
              ByteBlobs = Dictionary<_, _>(metadata.File.Count)
              Content = ChunkWriter(LinkedList<_>().AddFirst(Array.zeroCreate size)) }
        let writer = BlobWriter(metadata, blob.Content)
        let inline blobIndex pos item (dict: Dictionary<_, BlobIndex>) =
            let size = uint32(blob.Content.Position - pos)
            dict.Item <- item, { BlobIndex.Index = offset; BlobIndex.Size = size }
            offset <- offset + size + BlobSize.ofUnsigned size

        for field in metadata.Field.Rows do
            let signature = field.Signature
            if not (blob.Field.ContainsKey signature) then
                let pos = writer.Position
                writer.FieldSig signature
                blobIndex pos signature blob.Field

        for method in metadata.MethodDef.Rows do
            let signature = method.Signature
            if not (blob.MethodDef.ContainsKey signature) then
                let pos = writer.Position
                writer.Writer.WriteU1 signature.Flags

                //match signature.CallingConventions with
                //| MethodCallingConventions.Generic count -> invalidOp "TODO: Write number of generic parameters."
                //| _ -> ()

                writer.CompressedUnsigned signature.Parameters.Length // ParamCount
                writer.RetType signature.ReturnType
                writer.Parameters signature.Parameters
                blobIndex pos signature blob.MethodDef

        for memberRef in metadata.MemberRef.Rows do
            if not (blob.MemberRef.ContainsKey memberRef) then
                let pos = writer.Position

                match memberRef with
                | MethodRefDefault { Signature = signature } ->
                    writer.Writer.WriteU1 signature.CallingConventions
                    writer.CompressedUnsigned signature.Parameters.Length // ParamCount
                    writer.RetType signature.ReturnType
                    writer.Parameters signature.Parameters
                | MethodRefGeneric { Signature = signature } ->
                    writer.Writer.WriteU1 signature.CallingConventions
                    writer.CompressedUnsigned signature.GenParamCount
                    writer.CompressedUnsigned signature.Parameters.Length // ParamCount
                    writer.RetType signature.ReturnType
                    writer.Parameters signature.Parameters
                | MethodRefVarArg { Signature = signature } ->
                    writer.Writer.WriteU1 signature.CallingConventions
                    writer.CompressedUnsigned signature.ParamCount
                    writer.RetType signature.ReturnType
                    writer.Parameters signature.Parameters
                    if not signature.VarArgParameters.IsEmpty then
                        writer.Writer.WriteU1 ElementType.Sentinel
                        writer.Parameters signature.VarArgParameters
                | FieldRef { Signature = signature } -> writer.FieldSig signature

                blobIndex pos memberRef blob.MemberRef

        for { Value = value } in metadata.CustomAttribute do
            match value with
            | Some signature when not (blob.CustomAttribute.ContainsKey signature) ->
                let pos = writer.Position
                writer.Writer.WriteU2 1us // Prolog
                for arg in signature.FixedArg do
                    writer.FixedArg arg
                writer.Writer.WriteU2 signature.NamedArg.Length // NumNamed
                for arg in signature.NamedArg do
                    failwithf "TODO: Implement writing of named arguments for custom attributes"
                blobIndex pos signature blob.CustomAttribute
            | _ -> ()

        for row in metadata.Property.Rows do
            let signature = row.Type
            if not (blob.Property.ContainsKey signature) then
                let pos = writer.Position
                let property = if signature.HasThis then 0x28uy else 0x8uy
                writer.Writer.WriteU1 property
                writer.CompressedUnsigned signature.Parameters.Length // ParamCount
                writer.CustomMod signature.CustomMod
                writer.EncodedType signature.Type
                writer.Parameters signature.Parameters
                blobIndex pos signature blob.Property

        for typeSpec in metadata.TypeSpec.Rows do
            let signature = typeSpec.Signature
            if not (blob.TypeSpec.ContainsKey signature) then
                let pos = writer.Position

                match signature with
                | TypeSpec.GenericInst inst -> writer.GenericInst inst
                // TODO: Factor out common code shared between writing of TypeSpec and encoded Type.
                | TypeSpec.MVar num ->
                    writer.Writer.WriteU1 ElementType.MVar
                    writer.CompressedUnsigned num
                | TypeSpec.Var num ->
                    writer.Writer.WriteU1 ElementType.Var
                    writer.CompressedUnsigned num

                blobIndex pos signature blob.TypeSpec

        for methodSpec in metadata.MethodSpec.Rows do
            let inst = methodSpec.Instantiation
            if not (blob.MethodSpec.ContainsKey inst) then
                let pos = writer.Position
                writer.Writer.WriteU1 0xAuy // GENERICINST
                writer.CompressedUnsigned inst.Count
                for gparam in inst.ToImmutableArray() do writer.EncodedType gparam
                blobIndex pos inst blob.MethodSpec

        for { PublicKeyOrToken = token } in metadata.AssemblyRef.Rows do
            if not (blob.PublicKeyTokens.ContainsKey token) then
                let pos = writer.Position
                match token with
                | PublicKeyToken(b1, b2, b3, b4, b5, b6, b7, b8) ->
                    writer.Writer.WriteU1 b1
                    writer.Writer.WriteU1 b2
                    writer.Writer.WriteU1 b3
                    writer.Writer.WriteU1 b4
                    writer.Writer.WriteU1 b5
                    writer.Writer.WriteU1 b6
                    writer.Writer.WriteU1 b7
                    writer.Writer.WriteU1 b8
                | _ -> failwithf "Unable to write unsupported public key token %A" token
                blobIndex pos token blob.PublicKeyTokens

        for { File.HashValue = hashValue } in metadata.File.Rows do 
            if not (blob.ByteBlobs.ContainsKey hashValue) then
                writer.Writer.WriteBytes hashValue

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

    let writeBlob (blobs: BlobHeap) metadata (writer: ChunkWriter) =
        writer.WriteU1 0uy

        let mutable chunk, i = blobs.Content.Chunk.List.First, 0
        let writeAll (dict: Dictionary<_, BlobIndex>) =
            let writer' = BlobWriter(metadata, writer)
            for KeyValue(_, index) in dict do
                writer'.CompressedUnsigned index.Size
                let struct(chunk', i') = writer.WriteBytes(chunk, i, index.Size)
                chunk <- chunk'
                i <- i'

        writeAll blobs.Field
        writeAll blobs.MethodDef
        writeAll blobs.MemberRef
        writeAll blobs.CustomAttribute
        writeAll blobs.Property
        writeAll blobs.TypeSpec
        writeAll blobs.MethodSpec
        writeAll blobs.PublicKeyTokens
        writeAll blobs.ByteBlobs

    let writeUS (us: UserStringHeap) metadata (writer: ChunkWriter) =
        let writer' = BlobWriter(metadata, writer)
        writer.WriteU1 0uy
        for str in us do
            let { BlobIndex.Size = size } = us.IndexOf str
            writer'.CompressedUnsigned size
            Encoding.Unicode.GetBytes str |> writer.WriteBytes
            writer.WriteU1 0uy // TODO: Determine value of terminal byte.

type internal Heap<'T when 'T : equality> = Heap.Lookup<'T>
