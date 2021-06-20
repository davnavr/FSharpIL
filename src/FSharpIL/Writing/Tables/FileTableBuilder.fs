namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type FileTableBuilder internal () =
    let rows = RowSet<FileRow>()
    interface ITableBuilder<FileRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, _, row, wr) =
            wr.WriteLE(uint32 row.Flags)
            StreamOffset.writeString &wr hsizes row.Name.Offset.Offset
            StreamOffset.writeBlob &wr hsizes row.HashValue
