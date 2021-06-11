namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type FieldTableBuilder internal () =
    let rows = RangedRowList<FieldRow>()
    interface ITableBuilder<FieldRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, _, row, wr) =
            wr.WriteLE(uint16 row.Flags)
            StreamOffset.writeString &wr hsizes row.Name
            StreamOffset.writeBlob &wr hsizes row.Signature.FieldSig
