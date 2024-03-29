﻿[<RequireQualifiedAccess>]
module FSharpIL.Writing.WriteCli

open System.Collections.Generic

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.PortableExecutable

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
      [<DefaultValue>] mutable VTableFixups: RvaAndSizeWriter
      [<DefaultValue>] mutable MethodBodies: Rva
      [<DefaultValue>] mutable EmbeddedData: Rva }

    member this.CurrentRva(section: byref<ChunkedMemoryBuilder>) = this.StartRva + (section.Length - this.StartOffset)

type RvaAndSizeWriter with
    member this.StartOffset = this.start
    member this.WriteRva({ StartRva = start; StartOffset = start' }, offset) =
        this.start <- offset
        this.builder.WriteLE(uint32 start + (offset - start'))
    member this.WriteSize offset = this.builder.WriteLE(offset - this.start)

/// Writes the CLI header (II.25.3.3).
let header info (wr: byref<ChunkedMemoryBuilder>) =
    let header = info.Builder.Header
    wr.WriteLE Magic.CliHeaderSize // Cb
    wr.WriteLE header.MajorRuntimeVersion
    wr.WriteLE header.MinorRuntimeVersion
    info.Metadata <- RvaAndSizeWriter &wr
    wr.WriteLE(uint32 info.Builder.HeaderFlags) // Flags
    wr.WriteLE(uint32 info.Builder.EntryPointToken)
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
    let headers = Array.zeroCreate<ChunkedMemoryBuilder> info.Streams.Count
    for i = 0 to headers.Length - 1 do
        headers.[i] <- wr.ReserveBytes 8
        wr.Write info.Streams.[i].StreamName

    for i = 0 to headers.Length - 1 do
        let stream = info.Streams.[i]
        let mutable header = &headers.[i]
        header.WriteLE(wr.Length - info.Metadata.StartOffset) // Offset

        let start = wr.Length
        stream.Serialize(&wr, info.MethodBodies, info.EmbeddedData)
        wr.AlignTo 4
        let size = wr.Length - start

        match stream.StreamLength with
        | ValueSome expected when expected > size || size - expected > 3u ->
            failwithf
                "The \"%s\" stream was expected to have a length of %i bytes, but the actual length was %i bytes"
                (System.Text.Encoding.ASCII.GetString(stream.StreamName.AsSpan()).TrimEnd '\000')
                expected
                size
        | _ -> ()

        header.WriteLE size // Size

let metadata (section: byref<ChunkedMemoryBuilder>) cliHeaderRva builder =
    let info =
        { Builder = builder
          StartRva = cliHeaderRva
          StartOffset = section.Length
          Streams =
            let streams = List<IStreamBuilder> 5
            streams.Add builder.Tables
            if not builder.Strings.IsEmpty then streams.Add builder.Strings
            if not builder.UserString.IsEmpty then streams.Add builder.UserString
            if not builder.Guid.IsEmpty then streams.Add builder.Guid
            if not builder.Blob.IsEmpty then streams.Add builder.Blob
            streams }

    header info &section

    // TODO: Write strong name signature.
    // StrongNameSignature

    // C# compiler places field data after Import Table contents.
    // Data is instead inserted here so RVAs are known before tables are written.
    info.EmbeddedData <- info.CurrentRva &section
    for data in builder.EmbeddedData do section.Write data

    info.MethodBodies <- info.CurrentRva &section
    section.Write builder.MethodBodies

    root info &section
    streams info &section
    info.Metadata.WriteSize section.Length
