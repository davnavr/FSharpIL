/// Contains functions for writing offsets into metadata streams (II.24.2.6).
[<RequireQualifiedAccess>]
module FSharpIL.Writing.Tables.StreamOffset

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

let private writeOffset (wr: byref<ChunkedMemoryBuilder>) size (offset: uint32) =
    match size with
    | 2u -> wr.WriteLE(uint16 offset)
    | 4u
    | _ -> wr.WriteLE offset

let writeString (wr: byref<_>) (sizes: HeapSizes) { StringOffset = offset } =
    writeOffset &wr sizes.StringSize offset

let writeGuid (wr: byref<_>) (sizes: HeapSizes) { GuidIndex = index } =
    writeOffset &wr sizes.GuidSize index

let writeBlob (wr: byref<_>) (sizes: HeapSizes) { BlobOffset = offset } =
    writeOffset &wr sizes.GuidSize offset
