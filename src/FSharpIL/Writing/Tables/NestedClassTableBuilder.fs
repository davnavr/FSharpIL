namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type NestedClassTableBuilder internal () =
    let rows = RowSet<NestedClassRow>()
    interface ITableBuilder<NestedClassRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(_, tsizes, row, wr) =
            TableIndex.write &wr tsizes ValidTableFlags.TypeDef row.NestedClass
            TableIndex.write &wr tsizes ValidTableFlags.TypeDef row.EnclosingClass
