module internal FSharpIL.WriteCli

open FSharp.Core.Operators.Checked

open System.Collections.Generic
open System.Text

open FSharpIL.Bytes
open FSharpIL.Magic
open FSharpIL.Metadata

[<RequireQualifiedAccess>]
module Size =
    /// The length of the CLI header, in bytes.
    [<Literal>]
    let CliHeader = 0x48UL

/// <summary>Represents the <c>#Strings</c> metadata stream.</summary>
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
        // TODO: Determine if a Dictionary or ImmutableDictionary has faster lookup times.

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

    member _.ByteLength = 0UL

    member _.Index str =
        match str with
        | null
        | "" -> 0u
        | _ -> strings.Item str

type TablesInfo internal (metadata: CliMetadata, rva: uint64, strings: StringsHeap) =
    /// Size of the fields that make up the #~ stream, excluding the Rows and tables.
    [<Literal>]
    let FieldsSize = 24UL // Reserved, MajorVersion, MinorVersion, HeapSizes, Reserved, Valid, Sorted

    [<Literal>]
    let MaxSmallIndex = 0xFFFF

    let headerSize =
        let mutable size = FieldsSize + 4UL // Module
        if metadata.TypeRef.Count > 0 then size <- size + 4UL
        if metadata.TypeDef.Count > 0 then size <- size + 4UL
        if metadata.Field.Count > 0 then size <- size + 4UL
        if metadata.Method.Count > 0 then size <- size + 4UL

        if metadata.MemberRef.Count > 0 then size <- size + 4UL

        if metadata.CustomAttribute.Length > 0 then size <- size + 4UL

        if metadata.Assembly.IsSome then size <- size + 4UL
        if metadata.AssemblyRef.Count > 0 then size <- size + 4UL

        if metadata.NestedClass.Length > 0 then size <- size + 4UL
        size

    let heapSizes =
        let mutable bits = 0uy
        if strings.Count > MaxSmallIndex then bits <- bits ||| 1uy
        // TODO: Set size flags for other streams.
        bits

    let totalSize =
        headerSize
        // + metadata rows

    member _.Metadata = metadata

    /// Specifies the sizes of indexes into each stream.
    /// If a corresponding bit for a stream is set, then indexes or 4 bytes long instead of 2.
    member _.HeapSizes = heapSizes
    member _.Valid: uint64 = metadata.Valid
    member _.Sorted = 0UL

    member _.TotalSize = totalSize

    member _.String str =
        bytes {
            let i = strings.Index str
            if strings.Count > MaxSmallIndex
            then uint64 i
            else uint32 i
        }

[<Struct; System.Runtime.CompilerServices.IsReadOnly>]
type StreamHeader =
    { Offset: uint64
      Size: uint64
      Name: byte[] }

[<Sealed>]
type StreamsInfo internal (metadata: CliMetadata, headersRva: uint64) =
    let headersSize =
        12UL // #~
    let tablesRva = headersRva + headersSize
    let strings = StringsHeap metadata
    let tables = TablesInfo(metadata, tablesRva, strings)

    let totalSize =
        headersSize
        + tables.TotalSize
        + strings.ByteLength
        // + other stuff

    member val Headers =
        [|
            { Offset = tablesRva
              Size = tables.TotalSize
              Name = "#~\000\000"B }
        |]
    /// Size of all of the stream headers.
    member _.HeadersSize = headersSize
    member _.MetadataTables = tables

    member _.TotalSize = totalSize

[<Sealed>]
type RootInfo internal (metadata: CliMetadata, rootRva: uint64) =
    let version = MetadataVersion.toArray metadata.MetadataVersion
    /// Size of the CLI metadata root, excluding the stream headers and streams.
    let fieldsSize =
        uint64 CliSignature.Length
        + uint64 version.Length
        + 12UL // MajorVersion, MinorVersion, Reserved, Length
        + uint64 version.Length
        + 4UL // Flags, # of Streams
    let streams = StreamsInfo(metadata, fieldsSize + rootRva)

    member _.Rva = rootRva
    member _.Version = version
    member _.Streams = streams

    member _.TotalSize = fieldsSize + streams.TotalSize

[<Sealed>]
type CliInfo internal (metadata: CliMetadata, headerRva: uint64) =
    let strongNameSignatureRva = headerRva + Size.CliHeader
    let strongNameSignatureSize = uint64 metadata.Header.StrongNameSignature.Length
    let methodBodiesSize = 0UL
    let root = RootInfo(metadata, strongNameSignatureRva + strongNameSignatureSize + methodBodiesSize)

    let totalSize =
        Size.CliHeader
        + strongNameSignatureSize
        + methodBodiesSize
        + root.TotalSize

    member _.Metadata = metadata
    member _.TotalSize: uint64 = totalSize

    member _.Header = metadata.Header
    member _.HeaderRva = headerRva
    /// The RVA of the metadata root.
    member _.MetadataRoot = root
    member _.StrongNameSignatureRva = uint32 strongNameSignatureRva
    member _.StrongNameSignatureSize = uint32 strongNameSignatureSize

/// Writes the CLI header (II.25.3.3).
let header (info: CliInfo) =
    bytes {
        let header = info.Header
        Size.CliHeader // Cb
        header.MajorRuntimeVersion
        header.MinorRuntimeVersion

        uint32 info.MetadataRoot.Rva
        uint32 info.MetadataRoot.TotalSize // Size of metadata

        uint32 info.Metadata.HeaderFlags

        0u // EntryPointToken

        0u // RVA of Resources
        0u // Size of Resources

        if header.StrongNameSignature.IsEmpty
        then 0UL
        else
            info.StrongNameSignatureRva
            info.StrongNameSignatureSize

        0UL // CodeManagerTable
        0UL // VTableFixups
        0UL // ExportAddressTableJumps
        0UL // ManagedNativeHeader
    }

/// Writes the method bodies.
let bodies (info: CliInfo) =
    bytes {
        // TODO: Write method bodies.
        ()
    }

/// Writes the CLI metadata root (II.24.2.1) and the stream headers (II.24.2.2).
let root (info: CliInfo) =
    bytes {
        let root = info.MetadataRoot
        CliSignature
        1us // MajorVersion
        1us // MinorVersion
        0u // Reserved
        uint32 root.Version.Length
        root.Version
        0us // Flags

        let streams = info.MetadataRoot.Streams
        uint16 streams.Headers.Length
        for header in streams.Headers do
            if header.Name.Length % 4 <> 0 then
                invalidOp "Stream header names should be padded to the nearest four byte boundary."
            header.Offset
            header.Size
            header.Name
    }

let tables (tables: TablesInfo) =
    bytes {
        0u // Reserved
        2uy // MajorVersion
        0uy // MinorVersion
        tables.HeapSizes
        1uy
        tables.Valid
        tables.Sorted

        // Rows
        let metadata = tables.Metadata
        1u // Module
        if metadata.TypeRef.Count > 0 then uint32 metadata.TypeRef.Count
        if metadata.TypeDef.Count > 0 then uint32 metadata.TypeDef.Count
        if metadata.Field.Count > 0 then uint32 metadata.Field.Count
        if metadata.Method.Count > 0 then uint32 metadata.Method.Count

        if metadata.MemberRef.Count > 0 then uint32 metadata.MemberRef.Count

        if metadata.CustomAttribute.Length > 0 then uint32 metadata.CustomAttribute.Length

        if metadata.Assembly.IsSome then 1u
        if metadata.AssemblyRef.Count > 0 then uint32 metadata.AssemblyRef.Count

        if metadata.NestedClass.Length > 0 then uint32 metadata.NestedClass.Length

        // Module
        // 0us // Generation
        // TODO: Determine size of indexes (HeapSizes)
        // metadata.Module.Name
        // metadata.Module.Mvid
        // EncId
        // Encbaseid

        // TODO: Write tables
        // NOTE: Rows come right after each other. TypeDef EX: Flags, TypeName, Namespace, Extends, FieldList, MethodList
        ()
    }

let streams (info: CliInfo) =
    bytes {
        let streams = info.MetadataRoot.Streams

        tables streams.MetadataTables

        // TODO: Write #Strings, #US, #GUID, and #Blob streams.
    }

/// Writes the entirety of the CLI metadata to the specified writer.
let metadata (info: CliInfo) =
    bytes {
        header info
        info.Header.StrongNameSignature
        bodies info
        root info
        streams info
    }
