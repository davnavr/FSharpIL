namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type InterfaceImplTableBuilder internal () =
    let rows = RowSet<InterfaceImplRow>()
    interface ITableBuilder<InterfaceImplRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(_, tsizes, row, wr) =
            TableIndex.write &wr tsizes ValidTableFlags.TypeDef row.Class
            FSharpIL.Utilities.Fail.noImpl "TODO: coded index writing" row.Interface
