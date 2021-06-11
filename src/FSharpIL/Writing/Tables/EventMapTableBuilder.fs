namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type EventMapTableBuilder internal () =
    let rows = RowSet<EventMapRow>()
    interface ITableBuilder<EventMapRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(_, tsizes, row, wr) =
            TableIndex.write &wr tsizes ValidTableFlags.TypeDef row.Parent
            TableIndex.write &wr tsizes ValidTableFlags.Event row.EventList
