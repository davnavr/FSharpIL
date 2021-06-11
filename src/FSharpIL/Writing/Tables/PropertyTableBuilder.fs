namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type PropertyTableBuilder internal () =
    let rows = RangedRowList<PropertyRow>()
    interface ITableBuilder<PropertyRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, _, row, wr) =
            wr.WriteLE(uint16 row.Flags)
            StreamOffset.writeString &wr hsizes row.Name
            StreamOffset.writeBlob &wr hsizes row.Type.PropertySig
