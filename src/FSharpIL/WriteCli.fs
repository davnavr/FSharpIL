module internal FSharpIL.WriteCli

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
      mutable MethodBodiesSize: uint32 }

/// Writes the CLI header (II.25.3.3).
let header (info: CliInfo) (writer: ChunkWriter) =
    let header = info.Metadata.Header
    let strongNameSignatureRva = info.HeaderRva + Size.CliHeader
    let methodBodiesRva = strongNameSignatureRva + uint32 info.Metadata.Header.StrongNameSignature.Length
    writer.WriteU4 Size.CliHeader
    writer.WriteU2 header.MajorRuntimeVersion
    writer.WriteU2 header.MinorRuntimeVersion

    // MetaData
    writer.WriteU4(methodBodiesRva + info.MethodBodiesSize)
    writer.WriteU4 info.MetadataSize

    writer.WriteU4 info.Metadata.HeaderFlags // Flags
    writer.WriteU4 0u // EntryPointToken // TODO: Figure out what this token value should be. Is an index into the MethodDef table allowed?

    // Resources
    writer.WriteU4 0u
    writer.WriteU4 0u

    // StrongNameSignature
    writer.WriteU4 strongNameSignatureRva
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

/// Writes the CLI metadata root (II.24.2.1) and the stream headers (II.24.2.2).
let root (cli: CliMetadata) (writer: ChunkWriter) =
    writer.WriteBytes Magic.CliSignature
    writer.WriteU2 1us // MajorVersion
    writer.WriteU2 1us // MinorVersion
    writer.WriteU4 0u // Reserved
    writer.WriteU4 cli.MetadataVersion.Length
    MetadataVersion.toArray cli.MetadataVersion |> writer.WriteBytes
    writer.WriteU2 0us // Flags

    // TODO: Write stream headers.

/// Writes the entirety of the CLI metadata to the specified writer.
let metadata (cli: CliMetadata) (headerRva: uint32) (content: ChunkList) =
    let info =
        { HeaderRva = headerRva
          Metadata = cli
          MetadataSize = Unchecked.defaultof<uint32>
          MethodBodiesSize = Unchecked.defaultof<uint32> }
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
    ChunkWriter(content.Tail.Value, content.Tail.Value.Data.Length) |> root cli
    // TODO: Write metadata here.
    info.MetadataSize <- content.PopSize()

    ChunkWriter(tail, tail.Data.Length) |> header info
    ()
