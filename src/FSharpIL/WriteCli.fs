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
    let CliHeader = 0x48UL

type StreamHeader =
    { Offset: uint32
      Size: uint32
      Name: byte[] }

/// Writes the entirety of the CLI metadata to the specified writer.
let metadata (cli: CliMetadata) (writer: ChunkWriter) =
    ()
