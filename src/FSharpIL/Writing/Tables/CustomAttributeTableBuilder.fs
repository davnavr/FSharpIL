namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type CustomAttributeTableBuilder internal () =
    let rows = RowList<CustomAttributeRow>()
    interface ITableBuilder<CustomAttributeRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            CodedIndex.write &wr tsizes &CodedIndexKinds.HasCustomAttribute row.Parent
            CodedIndex.write &wr tsizes &CodedIndexKinds.CustomAttributeType row.Type
            StreamOffset.writeBlob &wr hsizes row.Value.CustomAttrib
