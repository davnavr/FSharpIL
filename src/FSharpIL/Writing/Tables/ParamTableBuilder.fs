namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type ParamTableBuilder internal () =
    let rows = RangedRowList<ParamRow>()
    interface ITableBuilder<ParamRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, _, row, wr) =
            wr.WriteLE(uint16 row.Flags)
            wr.WriteLE row.Sequence
            StreamOffset.writeString &wr hsizes row.Name
