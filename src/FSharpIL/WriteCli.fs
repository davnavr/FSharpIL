module internal FSharpIL.WriteCli

open FSharp.Core.Operators.Checked

open System
open System.Collections.Generic
open System.Text

open FSharpIL.Metadata
open FSharpIL.Metadata.Heaps
open FSharpIL.Writing

[<RequireQualifiedAccess>]
module Size =
    /// The length of the CLI header, in bytes.
    [<Literal>]
    let CliHeader = 0x48u

[<Sealed>]
type CodedIndex<'T> internal (count: int32, n: int32, indexer: 'T -> uint32 * uint32) =
    let large = count > (65535 <<< n)

    member val IndexSize = if large then 4 else 2

    member _.IndexOf (item: 'T) =
        let index, tag = indexer item
        (index <<< n) ||| tag
    
    member this.WriteIndex(item, writer: ChunkWriterOld) =
        let index = this.IndexOf item
        if large then writer.WriteU4 index else writer.WriteU2 index

let codedIndex count n indexer = CodedIndex<_>(count, n, indexer)

[<ReferenceEquality; NoComparison>]
type CliInfo =
    { HeaderRva: uint32
      Cli: CliMetadata
      /// Specifies the RVA and size of the CLI metadata (II.25.3.3).
      mutable Metadata: ChunkWriter
      /// Specifies the RVA and size of the "hash data for this PE file" (II.25.3.3).
      mutable StrongNameSignature: ChunkWriter
      StringsStream: Heap<string>
      // US
      GuidStream: Heap<Guid>
      BlobStream: BlobHeap }

/// Writes the CLI header (II.25.3.3).
let header info (writer: ChunkWriter) =
    let header = info.Cli.Header
    writer.WriteU4 Size.CliHeader
    writer.WriteU2 header.MajorRuntimeVersion
    writer.WriteU2 header.MinorRuntimeVersion

    // MetaData
    info.Metadata <- writer.CreateWriter()
    writer.SkipBytes 8

    writer.WriteU4 info.Cli.HeaderFlags // Flags
    writer.WriteU4 0u // EntryPointToken // TODO: Figure out what this token value should be. Is an index into the MethodDef table allowed?

    // Resources
    writer.WriteU4 0u
    writer.WriteU4 0u

    // StrongNameSignature
    info.StrongNameSignature <- writer.CreateWriter()
    writer.SkipBytes 8

    writer.WriteU8 0UL // CodeManagerTable

    // VTableFixups
    writer.WriteU4 0u
    writer.WriteU4 0u

    writer.WriteU8 0UL // ExportAddressTableJumps
    writer.WriteU8 0UL // ManagedNativeHeader

/// Writes the method bodies.
let bodies (info: CliInfo) (writer: ChunkWriter) =
    ()

/// Writes the contents of the #~ stream (II.24.2.6).
let tables (info: CliInfo) (writer: ChunkWriter) =
    ()

/// Writes a stream header in the CLI metadata root (II.24.2.2).
let streamHeader (writer: ChunkWriter) name =
    if Array.length name % 4 <> 0 then
        invalidArg (nameof name) "The length of the stream header name must be a multiple of four."
    let header = writer.CreateWriter()
    let name' = header.CreateWriter()
    name'.SkipBytes 8
    name'.WriteBytes name
    header

let stream (offset: byref<uint32>) (header: ChunkWriter) (writer: ChunkWriter) content =
    let heap = writer.CreateWriter()
    content writer
    // TODO: Align to four-byte boundary.
    let size = heap.Size
    header.WriteU4 offset
    header.WriteU4 size
    offset <- offset + size

/// Writes the CLI metadata root (II.24.2.1) and the stream headers (II.24.2.2).
let root (info: CliInfo) (writer: ChunkWriter) =
    let version = MetadataVersion.toArray info.Cli.MetadataVersion
    writer.WriteBytes Magic.CliSignature
    writer.WriteU2 1us // MajorVersion
    writer.WriteU2 1us // MinorVersion
    writer.WriteU4 0u // Reserved
    writer.WriteU4 version.Length
    writer.WriteBytes version
    writer.WriteU2 0us // Flags

    do // Streams
        let mutable count = 3u// #~, #Strings, #GUID
        // TODO: Include #US stream in stream count.
        if info.BlobStream.Count > 0 then count <- count + 1u

        writer.WriteU2 count

    let mutable offset = writer.Size

    // Since the ECMA-335 specification doesn't specify the order of the streams,
    // this implementation can get away with writing the #~ stream last.

    // Stream headers
    let strings = streamHeader writer "#Strings\000\000\000\000"B
    let us =
        // if info. // TODO: Write #US stream header
        // then streamHeader "#US\000"B
        Unchecked.defaultof<ChunkWriter>
    let guid = streamHeader writer "#GUID\000\000\000"B
    let blob =
        if info.BlobStream.Count > 0
        then streamHeader writer "#Blob\000\000\000"B
        else Unchecked.defaultof<ChunkWriter>
    let metadata = streamHeader writer "#~\000\000"B

    // #Strings
    stream &offset strings writer (invalidOp "Write strings")

    do // #US
        // if info.
        ()

    // #GUID
    stream &offset guid writer (invalidOp "Write guids")

    if blob <> Unchecked.defaultof<ChunkWriter> then
        stream &offset blob writer (invalidOp "Write blobs")

    // #~
    stream &offset metadata writer (tables info)

/// Writes the entirety of the CLI metadata to the specified writer.
let metadata (cli: CliMetadata) (headerRva: uint32) (section: ChunkWriter) =
    let writer = section.CreateWriter()
    let info =
        { HeaderRva = headerRva
          Cli = cli
          Metadata = Unchecked.defaultof<ChunkWriter>
          StrongNameSignature = Unchecked.defaultof<ChunkWriter>
          StringsStream = Heap.strings cli
          GuidStream = Heap.guid cli
          BlobStream = BlobHeap cli }
    let mutable rva = headerRva

    header info writer
    rva <- rva + Size.CliHeader

    do // Strong Name Signature
        info.StrongNameSignature.WriteU4 rva
        writer.ResetSize()
        writer.WriteBytes(cli.Header.StrongNameSignature)
        let size = writer.Size
        info.StrongNameSignature.WriteU4 size
        rva <- rva + size

    do // Method Bodies
        writer.ResetSize()
        bodies info writer
        rva <- rva + writer.Size

    do // CLI metadata
        info.Metadata.WriteU4 rva
        writer.ResetSize()
        root info writer
        info.Metadata.WriteU4 writer.Size
