﻿module internal FSharpIL.WriteCli

open FSharp.Core.Operators.Checked

open System
open System.Text

open FSharpIL.Metadata
open FSharpIL.Metadata.Heaps
open FSharpIL.Writing

[<RequireQualifiedAccess>]
module Size =
    /// The length of the CLI header, in bytes.
    [<Literal>]
    let CliHeader = 0x48u

type StreamHeader =
    { Offset: uint32
      Size: uint32
      Name: byte[] }

[<ReferenceEquality; NoComparison>]
type CliInfo =
    { HeaderRva: uint32
      Metadata: CliMetadata
      mutable MetadataSize: uint32
      mutable MethodBodiesSize: uint32
      StringsStream: StringsHeap
      GuidStream: GuidHeap }

    member this.MetadataRva = this.MethodBodiesRva + this.MethodBodiesSize
    member this.StrongNameSignatureRva = this.HeaderRva + Size.CliHeader
    member this.MethodBodiesRva = this.StrongNameSignatureRva + uint32 this.Metadata.Header.StrongNameSignature.Length

/// Writes the CLI header (II.25.3.3).
let header (info: CliInfo) (writer: ChunkWriter) =
    let header = info.Metadata.Header
    writer.WriteU4 Size.CliHeader
    writer.WriteU2 header.MajorRuntimeVersion
    writer.WriteU2 header.MinorRuntimeVersion

    // MetaData
    writer.WriteU4 info.MetadataRva
    writer.WriteU4 info.MetadataSize

    writer.WriteU4 info.Metadata.HeaderFlags // Flags
    writer.WriteU4 0u // EntryPointToken // TODO: Figure out what this token value should be. Is an index into the MethodDef table allowed?

    // Resources
    writer.WriteU4 0u
    writer.WriteU4 0u

    // StrongNameSignature
    writer.WriteU4 info.StrongNameSignatureRva
    writer.WriteU4 info.Metadata.Header.StrongNameSignature.Length

    writer.WriteU8 0UL // CodeManagerTable

    // VTableFixups
    writer.WriteU4 0u
    writer.WriteU4 0u

    writer.WriteU8 0UL // ExportAddressTableJumps
    writer.WriteU8 0UL // ManagedNativeHeader

/// Writes the method bodies.
let bodies (info: CliInfo) (content: ChunkList) =
    ()

/// Writes the contents of the #~ stream (II.24.2.6).
let tables (info: CliInfo) (content: ChunkList) =
    let headers = ChunkWriter.After(content.Tail.Value, 12)
    headers.WriteU4 0u // Reserved
    headers.WriteU1 2uy // MajorVersion
    headers.WriteU1 0uy // MinorVersion

    let heapSizes = // TODO: Move calculation of HeapSizes to CliMetadata class.
        let mutable bits = 0uy
        if info.StringsStream.IndexSize = 4 then bits <- bits ||| 1uy
        // #US
        if info.GuidStream.IndexSize = 4 then bits <- bits ||| 2uy
        // TODO: Set size flags for other streams.
        bits

    headers.WriteU1 heapSizes
    headers.WriteU1 1uy // Reserved
    headers.WriteU8 info.Metadata.Valid
    headers.WriteU8 0UL // Sorted

    // Rows
    for row in info.Metadata.RowCounts do
        let size = ChunkWriter.After(content.Tail.Value, 4)
        size.WriteU4 row

    // Tables
    let tables = info.Metadata

    let mdle =
        let size = 2 + info.StringsStream.IndexSize + (3 * info.GuidStream.IndexSize)
        ChunkWriter.After(content.Tail.Value, size)
    mdle.WriteU2 0us // Generation
    info.StringsStream.WriteIndex(tables.Module.Name, mdle)
    info.GuidStream.WriteIndex(tables.Module.Mvid, mdle)
    info.GuidStream.WriteZero mdle // EncId
    info.GuidStream.WriteZero mdle // Encbaseid

    // TODO: Write more tables.
    // NOTE: Rows come right after each other. TypeDef EX: Flags, TypeName, Namespace, Extends, FieldList, MethodList.
    ()

/// Writes the CLI metadata root (II.24.2.1) and the stream headers (II.24.2.2).
let root (info: CliInfo) (content: ChunkList) =
    let version = MetadataVersion.toArray info.Metadata.MetadataVersion
    let fieldsSize =
        Magic.CliSignature.Length
        + 12 // MajorVersion, MinorVersion, Reserved, Length
        + version.Length
        + 4 // Flags, # of Streams
    let writer = ChunkWriter.After(content.Tail.Value, fieldsSize)
    writer.WriteBytes Magic.CliSignature
    writer.WriteU2 1us // MajorVersion
    writer.WriteU2 1us // MinorVersion
    writer.WriteU4 0u // Reserved
    writer.WriteU4 version.Length
    writer.WriteBytes version
    writer.WriteU2 0us // Flags

    let streams =
        let mutable count = 2u // #Strings and #GUID
        // TODO: Include other streams in the count if they are not empty
        count
    writer.WriteU2 streams // Streams

    if writer.FreeBytes <> 0 then
        invalidOp "The metadata root did not fit in the current chunk."

    let mutable offset = uint32 fieldsSize

    // Stream headers
    let streamHeader name =
        if Array.length name % 4 <> 0 then
            invalidArg (nameof name) "The length of the stream header name must be a multiple of four."
        let location = content.AddAfter(content.Tail.Value, Array.zeroCreate<byte> 8)
        content.AddAfter(location, name) |> ignore
        offset <- offset + 8u + uint32 name.Length
        ChunkWriter(location)

    let metadata = streamHeader "#~\000\000"B
    let strings = streamHeader "#Strings\000\000\000\000"B
    // TODO: Write other stream headers.

    // #~ stream
    content.PushSize()
    tables info content
    let metadataSize = content.PopSize()
    offset <- offset + metadataSize
    metadata.WriteU4 offset
    metadata.WriteU4 metadataSize

    ()

/// Writes the entirety of the CLI metadata to the specified writer.
let metadata (cli: CliMetadata) (headerRva: uint32) (content: ChunkList) =
    let info =
        { HeaderRva = headerRva
          Metadata = cli
          MetadataSize = Unchecked.defaultof<uint32>
          MethodBodiesSize = Unchecked.defaultof<uint32>
          StringsStream = StringsHeap cli
          GuidStream = GuidHeap cli }
    let tail = content.Tail.Value

    // Since header requires the calculations of sizes, we write the data first and prepend the header afterward.

    let strongNameSignature = cli.Header.StrongNameSignature
    if not strongNameSignature.IsEmpty then
        content.AddAfter(tail, cli.Header.StrongNameSignature.AsSpan().ToArray()) |> ignore

    // Method bodies
    content.PushSize()
    bodies info content
    info.MethodBodiesSize <- content.PopSize()

    // CLI metadata
    content.PushSize()
    root info content
    info.MetadataSize <- content.PopSize()

    ChunkWriter.After(tail, int32 Size.CliHeader) |> header info
    ()
