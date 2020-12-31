module FSharpIL.WriteCli

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
    let tableSize = // NOTE: Must be rounded to a multiple of 4.
        24UL
        // Rows
        + 4UL
        // TODO: Add other rows
        // + Tables
        // TODO: + Size of Module table?
    let streamsSize =
        tableSize
        // TODO: + other streams, such as the string stream?

    member _.Version = version
    member _.Root = metadata
    member _.RootRva = rootRva
    member _.RootSize = rootSize
    member inline this.TableOffset = this.RootSize
    member _.TablesSize = tableSize
    member _.StreamsSize = streamsSize
    member _.TotalSize = rootSize + streamsSize

[<Sealed>]
type CliInfo (cli: CliHeader, headerRva: uint64) =
    let strongNameSignatureRva = headerRva + Size.CliHeader
    let strongNameSignatureSize = uint64 cli.StrongNameSignature.Length
    let methodBodiesSize = 0UL
    let metadataRva =
        strongNameSignatureRva
        + strongNameSignatureSize
        + methodBodiesSize
    let metadata = MetadataInfo (cli.Metadata, metadataRva)

    // TODO: Have separate Info class for CLI metadata.

    // TODO: Use Encoding.GetByteCount to determine the size of the Strings heap.

    let totalSize =
        Size.CliHeader
        + strongNameSignatureSize
        // + Method Bodies
        + metadata.TotalSize

    member _.Header = cli
    member _.HeaderRva = headerRva
    member _.StrongNameSignatureRva  = strongNameSignatureRva
    member _.StrongNameSignatureSize = strongNameSignatureSize
    member _.MethodBodiesSize = methodBodiesSize
    member _.MetadataRva = metadataRva
    member _.Metadata = metadata
    member _.TotalSize = totalSize

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
let inline streamHeader offset size (name: string) =
    bytes {
        uint32 offset // Note that the offset is relative to the RVA of the metadata.
        uint32 size
        let name' = Encoding.UTF8.GetBytes name
        let length = uint64 name'.Length |> Round.upTo 4UL
        name'
        empty (length - uint64 name'.Length)
    }

/// Writes the CLI metadata root (II.24.2.1).
let metadata (info: CliInfo) =
    /// Writes the stream headers.
    let headers =
        bytes {
            streamHeader info.Metadata.TableOffset info.Metadata.TablesSize "#~"
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

let tables (info: MetadataInfo) =
    let tables = info.Root.Streams.Tables
    let header =
        bytes {
            0u // Reserved
            tables.MajorVersion
            tables.MinorVersion
            0uy // HeapSizes // TODO: Determine what value this should have. Set flag 0x01 if string heap size is greater than 2 to the 16.
            0uy // Reserved
            tables.Valid
            0UL // Sorted // WHAT VALUE

            // Rows
            // NOTE: Integers are sorted by table number.
            1u // Module
        }
    bytes {
        header

        // Tables
        // bin.WriteU32 0 // Module
    }
    |> withLength info.TablesSize

let streams (info: CliInfo) =
    bytes {
        // #~ stream
        tables info.Metadata

        // TODO: Write #Strings, #US, #GUID, and #Blob streams

        // #Strings
        //if true = false then // TODO: Figure out the lengths of things first.
        //    0uy // empty string
        //    for str in strings do
        //        Encoding.UTF8.GetBytes str
        //        0uy // null-terminated
    }

let data (info: CliInfo) =
    bytes {
        header info
        info.Header.StrongNameSignature
        // TODO: Write method bodies.
        metadata info
        streams info
    }
    // TODO: Check that length is correct.
