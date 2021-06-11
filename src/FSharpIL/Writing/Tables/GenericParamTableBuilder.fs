namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type GenericParamTableBuilder internal () =
    let rows = RangedRowList<GenericParamRow>()
    interface ITableBuilder<GenericParamRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            wr.WriteLE row.Number
            wr.WriteLE(uint16 row.Flags)
            CodedIndex.write &wr tsizes &CodedIndexKinds.TypeOrMethodDef row.Owner
            StreamOffset.writeString &wr hsizes row.Name
