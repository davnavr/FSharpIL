namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type MethodSpecTableBuilder internal () =
    let rows = RowSet<MethodSpecRow>()
    interface ITableBuilder<MethodSpecRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            CodedIndex.write &wr tsizes &CodedIndexKinds.MethodDefOrRef row.Method
            StreamOffset.writeBlob &wr hsizes row.Instantiation.MethodSpec
