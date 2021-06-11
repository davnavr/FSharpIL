/// Contains functions for writing coded indices that can point into one of many possible tables (II.24.2.6).
[<RequireQualifiedAccess>]
module FSharpIL.Writing.Tables.CodedIndex

open FSharpIL.Metadata.Tables

let write<'Tag when 'Tag : enum<uint8>>
    (wr: byref<FSharpIL.ChunkedMemoryBuilder>)
    counts
    (kind: inref<CodedIndexKind<'Tag>>)
    (index: CodedIndex<'Tag>)
    =
    let i = (index.Index <<< kind.NumEncodingBits) ||| uint32(LanguagePrimitives.EnumToValue index.Tag)
    if kind.IsLarge counts
    then wr.WriteLE i
    else wr.WriteLE(uint16 i)
