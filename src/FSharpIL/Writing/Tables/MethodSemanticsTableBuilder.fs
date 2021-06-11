namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type MethodSemanticsTableBuilder internal () =
    let rows = RowSet<MethodSemanticsRow>()
    interface ITableBuilder<MethodSemanticsRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(_, tsizes, row, wr) =
            wr.WriteLE(uint16 row.Semantics)
            TableIndex.write &wr tsizes ValidTableFlags.MethodDef row.Method
            FSharpIL.Utilities.Fail.noImpl "TODO: coded index writing" row.Association
