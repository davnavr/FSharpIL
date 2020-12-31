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

    // TODO: Use Encoding.GetByteCount to determine the size of the Strings heap.

let header (info: CliInfo) =
    bytes {
        let header = info.Header
        uint32 Size.CliHeader
        header.MajorRuntimeVersion
        header.MinorRuntimeVersion
        uint32 info.MetaDataRva
        uint32 (info.MetaDataRootSize + info.MetaDataStreamsSize)
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
    // TODO: Check that length is correct.

let metadata (info: CliInfo) =
    let headers = ()
    let streams = ()

    bytes {
        let root = info.Header.Metadata
        CliSignature
        root.MajorVersion
        root.MinorVersion
        0u  // Reserved
        root.Version.Length
        info.MetaDataVersion
        0us // Flags
        root.Streams.Count

        // TODO: Write stream headers.
        // NOTE: stream header offsets are relative to info.MetaDataRva
    }
    // TODO: Check that length is correct.

let write (cli: CliHeader) (rva: uint64) =
    bytes {
        let info = CliInfo (cli, rva)
        header info
        cli.StrongNameSignature
        // TODO: Write method bodies.
    }
    // TODO: Check that length is correct.
