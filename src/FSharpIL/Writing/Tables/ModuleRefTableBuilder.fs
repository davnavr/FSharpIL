namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type ModuleRefTableBuilder internal () =
    let rows = RowList<ModuleRefRow>()
    interface ITableBuilder<ModuleRefRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, _, row, wr) = StreamOffset.writeString &wr hsizes row.Name
