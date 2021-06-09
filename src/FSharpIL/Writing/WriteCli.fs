[<RequireQualifiedAccess>]
module FSharpIL.Writing.WriteCli

open System.Collections.Generic

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata

type RvaAndSizeWriter = struct
    val mutable private builder: ChunkedMemoryBuilder
    [<DefaultValue>] val mutable private start: uint32
    new (builder: byref<ChunkedMemoryBuilder>) = { builder = builder.ReserveBytes 8 }
end

[<NoEquality; NoComparison>]
type CliInfo =
    { Builder: CliMetadataBuilder
      StartRva: Rva
      StartOffset: uint32
      Streams: List<IStreamBuilder>
      [<DefaultValue>] mutable Metadata: RvaAndSizeWriter
      [<DefaultValue>] mutable Resources: RvaAndSizeWriter
      [<DefaultValue>] mutable StrongNameSignature: RvaAndSizeWriter
      [<DefaultValue>] mutable VTableFixups: RvaAndSizeWriter }

type RvaAndSizeWriter with
    member this.StartOffset = this.start
    member this.WriteRva({ StartRva = start; StartOffset = start' }, offset) =
        this.start <- offset
        this.builder.WriteLE(uint32 start + (offset - start'))
    member this.WriteSize offset = this.builder.WriteLE(offset - this.start)

(*
The metadata is written in the following order:
- CLI Header (II.25.3.3)
- Strong Name Signature (II.6.2.1.3) (currently not implemented, would require writing of all metadata first to allow calculation of a hash)
- Method Bodies (II.25.4)
- CLI Metadata (II.24)
  - Metadata Root (II.24.2.1)
  - Stream Headers (II.24.2.2)
  - Streams
    - #~ (II.24.2.6 and II.22)
    - #Strings (II.24.2.3)
    - #US (II.24.2.4)
    - #GUID (II.24.2.5)
    - #Blob (II.24.2.4 and II.23.2)
*)

/// Writes the CLI header (II.25.3.3).
let header info (wr: byref<ChunkedMemoryBuilder>) =
    let header = info.Builder.Header
    wr.WriteLE Magic.cliHeaderSize // Cb
    wr.WriteLE header.MajorRuntimeVersion
    wr.WriteLE header.MinorRuntimeVersion
    info.Metadata <- RvaAndSizeWriter &wr
    wr.WriteLE(uint32 info.Builder.HeaderFlags) // Flags
    noImpl "TODO: Write EntryPointToken"
    info.Resources <- RvaAndSizeWriter &wr
    info.StrongNameSignature <- RvaAndSizeWriter &wr
    wr.WriteLE 0UL // CodeManagerTable
    info.VTableFixups <- RvaAndSizeWriter &wr
    wr.WriteLE 0UL // ExportAddressTableJumps
    wr.WriteLE 0UL // ManagedNativeHeader

/// Writes the CLI metadata root (II.24.2.1).
let root info (wr: byref<ChunkedMemoryBuilder>) =
    info.Metadata.WriteRva(info, wr.Length)
    let header = info.Builder.Root
    wr.Write Magic.metadataRootSignature
    wr.WriteLE header.MajorVersion
    wr.WriteLE header.MinorVersion
    wr.WriteLE header.Reserved
    wr.WriteLE header.Version.Length
    MetadataVersion.write &wr header.Version
    wr.WriteLE header.Flags
    wr.WriteLE(uint16 info.Streams.Count) // Streams

let streams info (wr: byref<ChunkedMemoryBuilder>) =
    let offsets = Array.zeroCreate<ChunkedMemoryBuilder> info.Streams.Count
    for i = 0 to offsets.Length - 1 do
        offsets.[i] <- wr.ReserveBytes 4
        let stream = info.Streams.[i]
        wr.WriteLE stream.StreamLength
        wr.Write stream.StreamName

    for i = 0 to offsets.Length - 1 do
        let stream = info.Streams.[i]
        offsets.[i].WriteLE(wr.Length - info.Metadata.StartOffset)
        let length = wr.Length
        stream.Serialize &wr
        if stream.StreamLength <> wr.Length - length then failwithf "Exceeded expected stream length (%i bytes)" stream.StreamLength

let metadata (section: byref<ChunkedMemoryBuilder>) cliHeaderRva builder =
    let info =
        { Builder = builder
          StartRva = cliHeaderRva
          StartOffset = section.Length
          Streams =
            let streams = List<IStreamBuilder> 5
            //streams.Add builder.Tables
            streams.Add builder.Strings
            //streams.Add builder.UserString
            streams.Add builder.Guid
            //streams.Add builder.Blob
            streams }
    header info &section

    // TODO: Write strong name signature.
    // StrongNameSignature

    noImpl "method bodies"

    // Metadata
    root info &section
    streams info &section
    info.Metadata.WriteSize section.Length
