/// <summary>
/// Contains functions for writing simple indices into tables (II.24.2.6), coded indices that can point into one of many
/// possible tables (II.24.2.6), or offsets into metadata streams (II.24.2.6).
/// </summary>
[<RequireQualifiedAccess>]
module private FSharpIL.Writing.WriteIndex

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

let table (wr: byref<FSharpIL.ChunkedMemoryBuilder>) counts table { TableIndex = i } =
    if TableIndex.isLarge table counts
    then wr.WriteLE i
    else wr.WriteLE(uint16 i)

let coded<'Tag when 'Tag : enum<uint8> and 'Tag : comparison>
    (wr: byref<FSharpIL.ChunkedMemoryBuilder>)
    counts
    (kind: inref<CodedIndexKind<'Tag>>)
    (index: CodedIndex<'Tag>)
    =
    let i = (index.Index <<< kind.NumEncodingBits) ||| uint32(LanguagePrimitives.EnumToValue index.Tag)
    if kind.IsLarge counts
    then wr.WriteLE i
    else wr.WriteLE(uint16 i)

let private offset (wr: byref<FSharpIL.ChunkedMemoryBuilder>) size (offset: uint32) =
    match size with
    | 2u -> wr.WriteLE(uint16 offset)
    | 4u
    | _ -> wr.WriteLE offset

let string (wr: byref<_>) (sizes: HeapSizes) { StringOffset = str } =
    offset &wr sizes.StringSize str

let guid (wr: byref<_>) (sizes: HeapSizes) { GuidIndex = index } =
    offset &wr sizes.GuidSize index

let blob (wr: byref<_>) (sizes: HeapSizes) { BlobOffset = blob } =
    offset &wr sizes.GuidSize blob
