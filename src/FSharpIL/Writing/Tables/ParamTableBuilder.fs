namespace FSharpIL.Writing.Tables

open FSharpIL.Utilities

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Struct>]
type internal ParamRowValidator =
    interface IRowRangeValidator<ParamRow> with
        member _.Validate _ = None
        member _.Duplicate _ = noImpl "Duplicate rows in the parameter table should not occur"

[<Sealed>]
type ParamTableBuilder internal () =
    static let comparer =
        { new System.Collections.Generic.IEqualityComparer<ParamRow> with
            member _.GetHashCode row = row.GetHashCode()
            member _.Equals(_, _) = false }
    let rows = RangedRowList<ParamRow, ParamRowValidator> comparer
    member _.Add parameters = ValidationResult.get(rows.TryAdd parameters) // TODO: Warning for gaps in sequence (5).
    member internal _.Next: TableIndex<ParamRow> = { TableIndex = uint32 rows.Count + 1u }
    interface ITableBuilder<ParamRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.SerializeRow(hsizes, _, row, wr) =
            wr.WriteLE(uint16 row.Flags)
            wr.WriteLE row.Sequence
            StreamOffset.writeString &wr hsizes row.Name
