﻿module internal FSharpIL.Metadata.Heaps

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
        + metadata.Method.Count

        + metadata.MemberRef.Count

        + assembly
        + (2 * metadata.AssemblyRef.Count)

        |> Dictionary<string, uint32>

    do
        let inline add str =
            match str with
            | null
            | "" -> ()
            | _ -> strings.TryAdd(str, uint32 strings.Count + 1u) |> ignore

        string metadata.Module.Name |> add

        for tref in metadata.TypeRef.Keys do
            string tref.Item.TypeName |> add
            string tref.Item.TypeNamespace |> add

        for tdef in metadata.TypeDef.Keys do
            string tdef.Item.TypeName |> add
            string tdef.Item.TypeNamespace |> add

        for field in metadata.Field.Keys do
            string field.Item.Name |> add

        for method in metadata.Method.Keys do
            string method.Item.Name |> add



        for mref in metadata.MemberRef.Keys do
            string mref.Item.MemberName |> add



        match metadata.Assembly with
        | Some assembly ->
            string assembly.Name |> add
            string assembly.Culture |> add
        | None -> ()

        for assembly in metadata.AssemblyRef.Keys do
            string assembly.Item.Name |> add
            string assembly.Item.Culture |> add
        ()

    member _.Count = strings.Count

    member _.ByteLength = 0UL // TODO: Calculate how many bytes the strings heap takes up.

    member _.IndexOf str =
        match str with
        | null
        | "" -> 0u
        | _ -> strings.Item str

    member this.WriteIndex(str, writer: ChunkWriter) =
        let i = this.IndexOf str
        if strings.Count > MaxSmallIndex
        then writer.WriteU8 i
        else writer.WriteU4 i

    member this.WriteIndex o = o.ToString() |> this.WriteIndex

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

    member _.Count = guids.Count

    member _.ByteLength = 16UL * uint64 guids.Count

    member _.IndexOf guid =
        if Guid.Empty = guid
        then 0u
        else guids.Item guid

    member this.WriteIndex(guid, writer: ChunkWriter) =
        let i = this.IndexOf guid
        if guids.Count > MaxSmallIndex
        then writer.WriteU8 i
        else writer.WriteU4 i
