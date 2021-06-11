namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type GenericParamConstraintTableBuilder internal () =
    let rows = RangedRowList<GenericParamConstraintRow>()
    interface ITableBuilder<GenericParamConstraintRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(_, tsizes, row, wr) =
            TableIndex.write &wr tsizes ValidTableFlags.GenericParam row.Owner
            CodedIndex.write &wr tsizes &CodedIndexKinds.TypeDefOrRef row.Constraint
