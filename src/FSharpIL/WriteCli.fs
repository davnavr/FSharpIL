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

type TablesInfo internal (metadata: CliMetadata) =
    member _.TotalSize = 0UL

[<Struct; System.Runtime.CompilerServices.IsReadOnly>]
type StreamHeader =
    { Offset: uint64
      Size: uint64
      Name: string }

[<Sealed>]
type StreamsInfo internal (metadata: CliMetadata, metadataRva: uint64) =
    let tables = TablesInfo metadata

    member val Headers =
        [|
            
        |]
    /// Size of all of the headers.
    member _.HeadersSize = 0UL
    member _.MetadataTables = tables
    member _.TotalSize = invalidOp "streams total size"

[<Sealed>]
type CliInfo internal (metadata: CliMetadata, headerRva: uint64) =
    let strongNameSignatureRva = headerRva + Size.CliHeader
    let strongNameSignatureSize = uint64 metadata.Header.StrongNameSignature.Length
    let methodBodiesSize =
        metadata.Method.Keys
        |> Seq.sumBy (fun handle -> uint64 handle.Item.Body.Length)
        |> Round.upTo 4UL
        // TODO: Is this rounded to a four byte boundary?
    let metadataRva =
        strongNameSignatureRva
        + strongNameSignatureSize
        + methodBodiesSize
    let streams = StreamsInfo(metadata, metadataRva)
    let metadataVersion = MetadataVersion.toArray metadata.MetadataVersion
    let rootSize =
        uint64 CliSignature.Length
        + uint64 metadataVersion.Length
        + 12UL // MajorVersion, MinorVersion, Reserved, Length
        + uint64 metadataVersion.Length
        + 4UL // Flags, Streams
        + streams.HeadersSize
    let metadataSize = rootSize + streams.TotalSize

    member _.Metadata = metadata
    member _.TotalSize: uint64 = invalidOp "TODO: Calculate size."

    member _.Header = metadata.Header
    member _.HeaderRva = headerRva
    /// The RVA of the metadata root.
    member _.MetadataRva = uint32 metadataRva
    member _.MetadataSize: uint32 = 0u
    member _.MetadataVersion = metadataVersion
    member _.MetadataStreams = streams
    member _.StrongNameSignatureRva = uint32 strongNameSignatureRva
    member _.StrongNameSignatureSize = uint32 strongNameSignatureSize

/// Writes the CLI header (II.25.3.3).
let header (info: CliInfo) =
    bytes {
        let header = info.Header
        Size.CliHeader // Cb
        header.MajorRuntimeVersion
        header.MinorRuntimeVersion

        info.MetadataRva
        info.MetadataSize // Size of metadata

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
        // TODO: Write method bodies
        ()
    }

/// Writes the CLI metadata root (II.24.2.1) and the stream headers (II.24.2.2).
let root (info: CliInfo) =
    bytes {
        let streams = info.MetadataStreams
        CliSignature
        1us // MajorVersion
        1us // MinorVersion
        0u // Reserved
        uint32 info.MetadataVersion.Length
        info.MetadataVersion
        0us // Flags
        uint16 streams.Headers.Length
        for header in streams.Headers do
            header.Offset
            header.Size
            let name = Encoding.ASCII.GetBytes header.Name
            name
            (Round.upTo 4 name.Length) - name.Length |> Array.zeroCreate<byte> // Padding to next 4-byte boundary
    }

let streams (info: CliInfo) =
    bytes {
        let streams = info.MetadataStreams
        // TODO: Write #~, #Strings, #US, #GUID, and #Blob streams
        ()
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
