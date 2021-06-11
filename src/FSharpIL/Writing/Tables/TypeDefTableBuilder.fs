namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type TypeDefTableBuilder internal () =
    let rows = RowSet<TypeDefRow>()
    interface ITableBuilder<TypeDefRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            wr.WriteLE(uint32 row.Flags)
            StreamOffset.writeString &wr hsizes row.TypeName
            StreamOffset.writeString &wr hsizes row.TypeNamespace
            FSharpIL.Utilities.Fail.noImpl "TODO: coded index writing" row.Extends
            TableIndex.write &wr tsizes ValidTableFlags.Field row.FieldList
            TableIndex.write &wr tsizes ValidTableFlags.Field row.MethodList
