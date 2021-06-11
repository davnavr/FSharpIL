namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type EventTableBuilder internal () =
    let rows = RangedRowList<EventRow>()
    interface ITableBuilder<EventRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            wr.WriteLE(uint16 row.EventFlags)
            StreamOffset.writeString &wr hsizes row.Name
            CodedIndex.write &wr tsizes &CodedIndexKinds.TypeDefOrRef row.EventType
