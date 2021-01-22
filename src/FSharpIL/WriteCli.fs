module internal FSharpIL.WriteCli

open FSharp.Core.Operators.Checked

open System
open System.Text

open FSharpIL.Bytes

open FSharpIL.Metadata
open FSharpIL.Metadata.Heaps

[<RequireQualifiedAccess>]
module Size =
    /// The length of the CLI header, in bytes.
    [<Literal>]
    let CliHeader = 0x48UL

type StreamHeader =
    { Offset: uint64
      Size: uint64
      Name: byte[] }

[<Sealed>]
type CliInfo (metadata: CliMetadata, headerRva: uint64) =
    member _.Metadata = metadata

    member _.HeaderRva = headerRva

/// Writes the CLI header (II.25.3.3).
let header (info: CliInfo, writer: ResizeByteWriter) =
    let header = info.Metadata.Header
    writer.WriteU8 Size.CliHeader
    writer.WriteU2 header.MajorRuntimeVersion
    writer.WriteU2 header.MinorRuntimeVersion

    ()

/// Writes the entirety of the CLI metadata to the specified writer.
let metadata (info: CliInfo, writer: ResizeByteWriter) =
    ()
