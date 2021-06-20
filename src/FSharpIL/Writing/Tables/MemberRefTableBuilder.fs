namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type MemberRefTableBuilder internal () =
    let rows = RowSet<MemberRefRow>() // TODO: Fix, don't want errors if duplicates exist, only warnings (6).
    interface ITableBuilder<MemberRefRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            CodedIndex.write &wr tsizes &CodedIndexKinds.MemberRefParent row.Class
            StreamOffset.writeString &wr hsizes row.Name.Offset
            StreamOffset.writeBlob &wr hsizes row.Signature.MemberRefSig
