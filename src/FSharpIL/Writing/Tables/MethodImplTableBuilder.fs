namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type MethodImplTableBuilder internal () =
    let rows = RowSet<MethodImplRow>()
    interface ITableBuilder<MethodImplRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(_, tsizes, row, wr) =
            TableIndex.write &wr tsizes ValidTableFlags.TypeDef row.Class
            CodedIndex.write &wr tsizes &CodedIndexKinds.MethodDefOrRef row.MethodBody
            CodedIndex.write &wr tsizes &CodedIndexKinds.MethodDefOrRef row.MethodDeclaration
