namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type ConstantTableBuilder internal () =
    let rows = RowSet<ConstantRow>()
    interface ITableBuilder<ConstantRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            wr.Write(uint8 row.Type)
            wr.Write 0uy // Padding
            CodedIndex.write &wr tsizes &CodedIndexKinds.HasConstant row.Parent
            StreamOffset.writeBlob &wr hsizes row.Value.Constant
