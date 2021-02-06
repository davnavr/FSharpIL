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

    // us

    /// <summary>Creates the <c>#Blob</c> metadata stream, which contains signatures (II.24.2.4).</summary>
    let blob (metadata: CliMetadata) =
        let blob =
            { 
              MethodDef = Dictionary<_, _> metadata.MethodDef.Count
              MethodRef = Dictionary<_, _> metadata.MemberRef.Count

              CustomAttribute = Dictionary<_, _> metadata.CustomAttribute.Length
              PublicKeyTokens = Dictionary<_, _> metadata.AssemblyRef.Count
              ByteLength = 1u }
        let inline index (dict: Dictionary<_, _>) key size =
            if size > 0u then
                let i = blob.ByteLength
                let index = { BlobIndex.Index = i; BlobIndex.Size = size }
                blob.ByteLength <- blob.ByteLength + index.TotalSize
                dict.Item <- key, index

        CliMetadata.iterBlobs
            (fun signature ->
                if not (blob.MethodDef.ContainsKey signature) then
                    BlobSize.ofMethodDefSignature signature |> index blob.MethodDef signature)
            (fun signature ->
                if not (blob.MethodRef.ContainsKey signature) then
                    BlobSize.ofMethodRefSignature signature |> index blob.MethodRef signature)
            (fun signature ->
                if not (blob.CustomAttribute.ContainsKey signature) then
                    BlobSize.ofCustomAttribute signature |> index blob.CustomAttribute signature)
            (fun token ->
                if not (blob.PublicKeyTokens.ContainsKey token) then
                    match token with
                    | _ when false -> 0u
                    | PublicKey key -> uint32 key.Length
                    | PublicKeyToken _ -> 8u
                    | NoPublicKey -> 0u
                    |> index blob.PublicKeyTokens token)
            metadata

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

    let writeBlob (blobs: BlobHeap) (metadata: CliMetadata) (writer: ChunkWriter) =
        let methodDef = HashSet<_> blobs.MethodDef.Count
        let methodRef = HashSet<_> blobs.MethodRef.Count
        let attributes = HashSet<_> blobs.CustomAttribute.Count
        let publicKeyTokens = HashSet<_> blobs.PublicKeyTokens.Count

        writer.WriteU1 0uy // Empty blob

        CliMetadata.iterBlobs
            (fun signature ->
                if methodDef.Add signature then
                    writer.WriteBlobSize blobs.MethodDef.[signature].Size
                    writer.WriteU1 signature.Flags

                    // match signature.CallingConventions with
                    // | MethodCallingConventions.Generic count -> invalidOp "TODO: Write number of generic parameters."

                    writer.WriteCompressed signature.Parameters.Length // ParamCount
                    writer.WriteRetType signature.ReturnType
                    writer.WriteParameters signature.Parameters)
            (fun signature ->
                if methodRef.Add signature then
                    writer.WriteBlobSize blobs.MethodRef.[signature].Index
                    writer.WriteU1 signature.CallingConventions
                    writer.WriteCompressed signature.Parameters.Length // ParamCount
                    writer.WriteRetType signature.ReturnType
                    writer.WriteParameters signature.Parameters
                    // TODO: Write SENTINEL and extra parameters.
                    if not signature.VarArgParameters.IsEmpty then
                        failwith "TODO: Implement writing of VarArg parameters")
            (fun signature ->
                if attributes.Add signature then
                    if signature.FixedArg.IsEmpty && signature.NamedArg.IsEmpty then
                        invalidOp "Attributes without any fixed or named arguments should use the empty blob instead."
                    writer.WriteBlobSize blobs.CustomAttribute.[signature].Size
                    writer.WriteU2 1us // Prolog
                    for arg in signature.FixedArg do
                        writer.WriteFixedArg arg
                    writer.WriteU2 signature.NamedArg.Length // NumNamed
                    for arg in signature.NamedArg do
                        failwithf "TODO: Implement writing of named arguments for custom attributes")
            (fun token ->
                if publicKeyTokens.Add token then
                    match token with
                    | PublicKeyToken(b1, b2, b3, b4, b5, b6, b7, b8) ->
                        writer.WriteBlobSize 8u
                        writer.WriteU1 b1
                        writer.WriteU1 b2
                        writer.WriteU1 b3
                        writer.WriteU1 b4
                        writer.WriteU1 b5
                        writer.WriteU1 b6
                        writer.WriteU1 b7
                        writer.WriteU1 b8
                    | _ -> failwithf "Invalid public key or token %A" token)
            metadata

type internal Heap<'T when 'T : equality> = Heap.Lookup<'T>
