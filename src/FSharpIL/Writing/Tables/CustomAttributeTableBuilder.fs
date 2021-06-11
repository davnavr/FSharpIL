namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type CustomAttributeTableBuilder internal () =
    let rows = RowSet<CustomAttributeRow>()
    interface ITableBuilder<CustomAttributeRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            FSharpIL.Utilities.Fail.noImpl "TODO: coded index writing" row.Parent
            FSharpIL.Utilities.Fail.noImpl "TODO: coded index writing" row.Type
            StreamOffset.writeBlob &wr hsizes row.Value.CustomAttrib
