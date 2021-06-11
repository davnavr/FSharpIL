/// Contains functions for writing simple indices into tables (II.24.2.6).
[<RequireQualifiedAccess>]
module FSharpIL.Writing.Tables.TableIndex

open FSharpIL.Metadata.Tables

let write (wr: byref<FSharpIL.ChunkedMemoryBuilder>) counts table { TableIndex = i } =
    if TableIndex.isLarge table counts
    then wr.WriteLE i
    else wr.WriteLE(uint16 i)
