namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type StandaloneSigTableBuilder internal () =
    let rows = RowList<StandaloneSigRow>()
    member _.Add (row: inref<_>) = rows.Add &row
    interface ITableBuilder<StandaloneSigRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, _, row, wr) = StreamOffset.writeBlob &wr hsizes row.Signature
