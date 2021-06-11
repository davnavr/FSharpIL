namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type TypeSpecTableBuilder internal () =
    let rows = RowSet<TypeSpecRow>()
    interface ITableBuilder<TypeSpecRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, _, row, wr) = StreamOffset.writeBlob &wr hsizes row.Signature
