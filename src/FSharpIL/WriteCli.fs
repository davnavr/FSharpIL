module internal FSharpIL.WriteCli

open FSharp.Core.Operators.Checked

open System
open System.Collections.Generic
open System.Text

open FSharpIL.Bytes
open FSharpIL.Magic
open FSharpIL.Metadata

[<RequireQualifiedAccess>]
module Size =
    /// The length of the CLI header.
    [<Literal>]
    let CliHeader = 0x48UL

[<Sealed>]
type MetadataInfo (metadata: MetadataRoot, rootRva: uint64) =
    let version = MetadataVersion.toArray metadata.Version
    let streamHeadersSize =
        12UL // #~
    let rootSize =
        uint64 CliSignature.Length
        + uint64 version.Length
        + streamHeadersSize
        + 16UL
    let tableSize =
        24UL
        // + Rows
        // + Tables
        // TODO: + Size of Module table?
    let streamsSize =
        tableSize
        // TODO: + other streams, such as the string stream?

    member _.Version = version
    member _.Root = metadata
    member _.RootRva = rootRva
    member _.RootSize = rootSize
    member _.StreamsSize = streamsSize

[<Sealed>]
type CliInfo (cli: CliHeader, headerRva: uint64) =
    let strongNameSignatureRva = headerRva + Size.CliHeader
    let strongNameSignatureSize = uint64 cli.StrongNameSignature.Length
    let methodBodiesSize = 0UL
    let metadataRva =
        strongNameSignatureRva
        + strongNameSignatureSize
        + methodBodiesSize

    // TODO: Have separate Info class for CLI metadata.

    member _.Header = cli
    member _.HeaderRva = headerRva
    member _.StrongNameSignatureRva  = strongNameSignatureRva
    member _.StrongNameSignatureSize = strongNameSignatureSize
    member _.MethodBodiesSize = methodBodiesSize
    member _.MetadataRva = metadataRva
    member val Metadata = MetadataInfo (cli.Metadata, metadataRva)

    // TODO: Use Encoding.GetByteCount to determine the size of the Strings heap.

let header (info: CliInfo) =
    bytes {
        let header = info.Header
        uint32 Size.CliHeader
        header.MajorRuntimeVersion
        header.MinorRuntimeVersion
        uint32 info.MetadataRva
        uint32 (info.Metadata.RootSize + info.Metadata.StreamsSize)
        uint32 header.Flags
        0u // EntryPointToken

        0u // RVA of Resources
        0u // Size of Resources

        if header.StrongNameSignature.IsEmpty
        then 0UL
        else
            uint32 info.StrongNameSignatureRva
            uint32 info.StrongNameSignatureSize

        0UL // CodeManagerTable
        0UL // VTableFixups // TODO: See if this needs to be assigned a value.
        0UL // ExportAddressTableJumps
        0UL // ManagedNativeHeader
    }
   |> withLength Size.CliHeader

/// Writes a single stream header (II.24.2.2).
let streamHeader (offset: uint32) (size: uint32) (name: string) =
    bytes {
        offset // Note that the offset is relative to the RVA of the metadata.
        size
        let name' = Encoding.UTF8.GetBytes name
        name'
        empty (uint64 name'.Length |> Round.upTo 4UL)
    }

/// Writes the CLI metadata root (II.24.2.1).
let metadata (info: CliInfo) =
    /// Writes the stream headers.
    let headers =
        bytes {
            streamHeader 0u 0u "#~"
            // TODO: Write other stream headers.
        }
    bytes {
        let root = info.Header.Metadata
        CliSignature
        root.MajorVersion
        root.MinorVersion
        0u  // Reserved
        root.Version.Length
        info.Metadata.Version
        0us // Flags
        root.Streams.Count
        headers
    }
    // TODO: Check that length is correct.

let streams (info: CliInfo) =
    bytes {
        let tables = info.Metadata.Root.Streams.Tables

        // #~ stream
        0u // Reserved
        tables.MajorVersion
        tables.MinorVersion
        0uy // HeapSizes // TODO: Determine what value this should have. Set flag 0x01 if string heap size is greater than 2 to the 16.
        0uy // Reserved
        tables.Valid
        0UL // Sorted // WHAT VALUE
        // Rows // TODO: Determine which integer in the array corresponds to which table.

        // Tables
        // bin.WriteU32 0 // Module

        // NOTE: Perhaps the Module table can be written after the fields of the #~ stream.

        // TODO: Write #Strings, #US, #GUID, and #Blob streams

        // #Strings
        //if true = false then // TODO: Figure out the lengths of things first.
        //    0uy // empty string
        //    for str in strings do
        //        Encoding.UTF8.GetBytes str
        //        0uy // null-terminated
    }

let write (cli: CliHeader) (rva: uint64) =
    bytes {
        let info = CliInfo (cli, rva)
        header info
        cli.StrongNameSignature
        // TODO: Write method bodies.
        metadata info
        streams info
    }
    // TODO: Check that length is correct.
