namespace FSharpIL.Writing.Tables.Collections

open System.Collections.Immutable

open FSharpIL.Metadata.Tables

/// Contains the rows of a metadata table, and allows the addition of duplicate rows.
[<Sealed>]
type RowList<'Row when 'Row : equality and 'Row : struct and 'Row :> ITableRow> internal () =
    let rows = ImmutableArray.CreateBuilder<'Row>()
    member _.Count = rows.Count
    member _.Add(row: inref<'Row>) =
        rows.Add row
        { TableIndex = uint32 rows.Count }
    member _.Item with get { TableIndex = i } = &rows.ItemRef(int32 i - 1)
