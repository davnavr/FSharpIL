namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type MethodDefTableBuilder internal () =
    let rows = RangedRowList<MethodDefRow>()
    interface ITableBuilder<MethodDefRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, tsizes, row, wr) =
            wr.WriteLE(uint32 row.Rva)
            wr.WriteLE(uint16 row.ImplFlags)
            wr.WriteLE(uint16 row.Flags)
            StreamOffset.writeString &wr hsizes row.Name
            StreamOffset.writeBlob &wr hsizes row.Signature.MethodDefSig
            TableIndex.write &wr tsizes ValidTableFlags.Param row.ParamList
