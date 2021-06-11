namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type MemberRefTableBuilder internal () =
    let rows = RowSet<MemberRefRow>()
    interface ITableBuilder<MemberRefRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            FSharpIL.Utilities.Fail.noImpl "TODO: coded index writing" row.Class
            StreamOffset.writeString &wr hsizes row.Name
            StreamOffset.writeBlob &wr hsizes row.Signature.MemberRefSig
