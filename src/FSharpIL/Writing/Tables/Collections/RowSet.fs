namespace FSharpIL.Writing.Tables.Collections

open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL.Metadata.Tables

/// Contains the rows of a metadata table, and prevents the addition of duplicate rows.
[<Sealed>]
type RowSet<'Row when 'Row : equality and 'Row : struct and 'Row :> ITableRow> internal (comparer: IEqualityComparer<'Row>) =
    let lookup = Dictionary<'Row, TableIndex<'Row>>(comparer) // TODO: Figure out how to force usage of inref for equality comparison.
    let rows = ImmutableArray.CreateBuilder<'Row>()
    internal new () = RowSet<'Row> EqualityComparer.Default
    member _.Count = rows.Count
    member _.TryAdd(row: inref<'Row>, offset: outref<TableIndex<'Row>>) =
        let offset' = { TableIndex = uint32 rows.Count + 1u }
        if lookup.TryAdd(row, offset') then
            offset <- offset'
            rows.Add row
            true
        else false
    member _.Item with get (i: TableIndex<'Row>) = &rows.ItemRef(int32 i.TableIndex - 1) // TODO: Avoid duplicate code with RowList
