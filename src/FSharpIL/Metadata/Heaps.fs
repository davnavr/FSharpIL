module internal FSharpIL.Metadata.Heaps

open System
open System.Collections.Generic

open FSharpIL.Writing

[<Literal>]
let MaxSmallIndex = 0xFFFF

// TODO: Determine if adding strings first and then allowing retrieval of index is faster than assigning an index to each string as it is written.
/// <summary>Represents the <c>#Strings</c> metadata stream (II.24.2.3).</summary>
[<Sealed>]
type StringsHeap internal (metadata: CliMetadata) = // NOTE: Appears to simply contain the strings with only null characters separating them.
    let strings =
        let assembly =
            match metadata.Assembly with
            | Some _ -> 2
            | None -> 0

        1 // Module
        + (2 * metadata.TypeRef.Count)
        + (2 * metadata.TypeDef.Count)
        + metadata.Field.Count
        + metadata.MethodDef.Count
        + metadata.Param.Length

        + metadata.MemberRef.Count

        + assembly
        + (2 * metadata.AssemblyRef.Count)

        |> Dictionary<string, uint32>

    do
        let inline add str =
            match str with
            | null
            | "" -> ()
            | _ -> strings.Item <- str, uint32 strings.Count + 1u

        string metadata.Module.Name |> add

        for tref in metadata.TypeRef.Items do
            string tref.TypeName |> add
            string tref.TypeNamespace |> add

        for tdef in metadata.TypeDef.Items do
            string tdef.TypeName |> add
            string tdef.TypeNamespace |> add

        for field in metadata.Field.Items do
            string field.Name |> add

        for method in metadata.MethodDef.Items do
            string method.Name |> add
            for param in method.ParamList do
                add param.ParamName


        for mref in metadata.MemberRef.Items do
            string mref.MemberName |> add



        match metadata.Assembly with
        | Some assembly ->
            string assembly.Name |> add
            string assembly.Culture |> add
        | None -> ()

        for assembly in metadata.AssemblyRef.Items do
            string assembly.Name |> add
            string assembly.Culture |> add
        ()

    member val Count = strings.Count

    member _.ByteLength = 0UL // TODO: Calculate how many bytes the strings heap takes up.

    member _.IndexOf str =
        match str with
        | null
        | "" -> 0u
        | _ -> strings.Item str

    member val IndexSize = if strings.Count > MaxSmallIndex then 4 else 2

    member this.WriteIndex(str, writer: ChunkWriter) =
        let i = this.IndexOf str
        if this.IndexSize = 4
        then writer.WriteU4 i
        else writer.WriteU2 i

    member this.WriteIndex(o, writer: ChunkWriter) = this.WriteIndex(o.ToString(), writer)

/// <summary>Represents the <c>#US</c> metadata stream (II.24.2.4).</summary>
[<Sealed>]
type UserStringHeap internal (metadata: CliMetadata) =
    let strings =
        1
        |> Dictionary<string, uint32>

    // NOTE: When writing this heap, see II.24.2.4 to see how lengths of the bytes are encoded.

/// <summary>Represents the <c>#GUID</c> metadata stream (II.24.2.5).</summary>
[<Sealed>]
type GuidHeap internal (metadata: CliMetadata) =
    let guids = Dictionary<Guid, uint32> 1

    do
        let inline add guid =
            guids.TryAdd(guid, uint32 guids.Count + 1u) |> ignore

        metadata.Module.Mvid |> add

    member val Count = guids.Count

    member _.ByteLength = 16UL * uint64 guids.Count

    member _.IndexOf guid =
        if Guid.Empty = guid
        then 0u
        else guids.Item guid

    member val IndexSize = if guids.Count > MaxSmallIndex then 4 else 2

    member this.WriteIndex(guid, writer: ChunkWriter) =
        let i = this.IndexOf guid
        if this.IndexSize = 4
        then writer.WriteU4 i
        else writer.WriteU2 i

    member this.WriteZero writer = this.WriteIndex(Guid.Empty, writer)

/// <summary>Represents the <c>#Blob</c> metadata stream (II.24.2.4).</summary>
[<Sealed>]
type BlobHeap internal (metadata: CliMetadata) =
    // let field = Dictionary<FieldSignature, uint32> metadata.Field.Count
    let methodDef = Dictionary<MethodDefSignature, uint32> metadata.MethodDef.Count

    // MemberRef
    let methodRef = Dictionary<MethodRefSignature, uint32> metadata.MemberRef.Count
    let fieldRef = ()

    // Couldn't find documentation indicating what the first index of the first blob is, so it is assumed that index 0 corresponds to the empty blob.
    do
        let mutable i = 1u

        // TODO: Add field signatures

        for row in metadata.MethodDef.Items do
            methodDef.Item <- row.Signature, i
            i <- i + 1u



        for row in metadata.MemberRef.Items do
            match row with
            | MethodRef method -> methodRef.Item <- method.Signature, i
            i <- i + 1u

        // TODO: Add other blobs.
        ()

    let count = methodDef.Count + methodRef.Count

    member _.IndexOf signature = methodDef.Item signature
    member _.IndexOf signature = methodRef.Item signature

    member val Count = count
    member val IndexSize = if count > MaxSmallIndex then 4 else 2

    member private this.WriteIndex(i: uint32, writer: ChunkWriter) =
        if this.IndexSize = 4
        then writer.WriteU4 i
        else writer.WriteU2 i

    member this.WriteIndex(signature: MethodDefSignature, writer) = this.WriteIndex(this.IndexOf signature, writer)
    member this.WriteIndex(signature: MethodRefSignature, writer) = this.WriteIndex(this.IndexOf signature, writer)
