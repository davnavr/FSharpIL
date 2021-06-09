[<RequireQualifiedAccess>]
module FSharpIL.Writing.WriteCli

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata

[<NoEquality; NoComparison>]
type CliInfo =
    { Builder: CliMetadataBuilder
      StartRva: Rva
      StartOffset: uint32
      [<DefaultValue>] mutable Metadata: ChunkedMemoryBuilder
      [<DefaultValue>] mutable Resources: ChunkedMemoryBuilder
      [<DefaultValue>] mutable StrongNameSignature: ChunkedMemoryBuilder
      [<DefaultValue>] mutable VTableFixups: ChunkedMemoryBuilder }

let rvaOf { StartRva = start; StartOffset = start' } offset = uint32 start + (offset - start')

(*
The metadata is written in the following order:
- CLI Header (II.25.3.3)
- Strong Name Signature (II.6.2.1.3) (currently not implemented, would require writing of all metadata first to allow calculation of a hash)
- Method Bodies (II.25.4)
- CLI Metadata (II.24)
  - Metadata Root (II.24.2.1)
  - Stream Headers (II.24.2.2)
  - Metadta Tables (II.22)
*)

/// Writes the CLI header (II.25.3.3).
let header info (wr: byref<ChunkedMemoryBuilder>) =
    let header = info.Builder.Header
    wr.WriteLE Magic.cliHeaderSize // Cb
    wr.WriteLE header.MajorRuntimeVersion
    wr.WriteLE header.MinorRuntimeVersion
    info.Metadata <- wr.ReserveBytes 8
    wr.WriteLE(uint32 info.Builder.HeaderFlags) // Flags
    noImpl "TODO: Write EntryPointToken"
    info.Resources <- wr.ReserveBytes 8
    info.StrongNameSignature <- wr.ReserveBytes 8
    wr.SkipBytes 8 // CodeManagerTable
    info.VTableFixups <- wr.ReserveBytes 8
    wr.SkipBytes 8 // ExportAddressTableJumps
    wr.SkipBytes 8 // ManagedNativeHeader

/// Writes the CLI metadata root (II.24.2.1).
let root info (wr: byref<ChunkedMemoryBuilder>) =
    info.Metadata.WriteLE(rvaOf info wr.Length)
    let header = info.Builder.Root
    wr.Write Magic.metadataRootSignature
    wr.WriteLE header.MajorVersion
    wr.WriteLE header.MinorVersion
    wr.WriteLE header.Reserved
    wr.WriteLE header.Version.Length
    MetadataVersion.write &wr header.Version
    wr.WriteLE header.Flags
    wr.WriteLE(noImpl "TODO: Get # of streams")

let metadata (section: byref<ChunkedMemoryBuilder>) cliHeaderRva builder =
    let info = { Builder = builder; StartRva = cliHeaderRva; StartOffset = section.Length }
    header info &section

    // TODO: Write strong name signature.

    noImpl "method bodies"

    root info &section
    noImpl "stream headers"
    ()
