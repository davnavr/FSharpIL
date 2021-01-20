module internal FSharpIL.WriteCli

open FSharp.Core.Operators.Checked

open System.Text

open FSharpIL.Bytes
open FSharpIL.Magic
open FSharpIL.Metadata

[<RequireQualifiedAccess>]
module Size =
    /// The length of the CLI header, in bytes.
    [<Literal>]
    let CliHeader = 0x48UL

type TablesInfo internal (metadata: CliMetadata, rva: uint64) =
    /// Size of the fields that make up the #~ stream, excluding the Rows and tables.
    [<Literal>]
    let FieldsSize = 24UL // Reserved, MajorVersion, MinorVersion, HeapSizes, Reserved, Valid, Sorted

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

    let totalSize =
        headerSize
        // + metadata rows

    member _.Metadata = metadata

    member _.HeapSizes = 0uy // TODO: Figure out what this should be.
    member _.Valid: uint64 = metadata.Valid
    member _.Sorted = 0UL

    member _.TotalSize = totalSize

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
    let tables = TablesInfo(metadata, tablesRva)

    let totalSize =
        headersSize
        + tables.TotalSize
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

        // TODO: Write tables
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
