namespace FSharpIL.Writing.Tables.Collections

open System
open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL.Utilities

open FSharpIL.Metadata.Tables

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type TableIndexRange<'Row when 'Row : struct and 'Row :> ITableRow> =
    internal { Start: int32; End: int32 }
    member this.Count = this.End - this.Start + 1
    member this.StartIndex = { TableIndex = uint32 this.Start }

[<Sealed>]
type RangedRowList<'Row when 'Row : struct and 'Row : equality and 'Row :> ITableRow> =
    val private rows: ImmutableArray<'Row>.Builder
    val private comparer: IEqualityComparer<'Row>
    internal new (comparer) = { rows = ImmutableArray.CreateBuilder(); comparer = comparer }
    internal new () = RangedRowList EqualityComparer.Default
    member this.Count = this.rows.Count
    member this.Item with get (i: TableIndex<'Row>) = &this.rows.ItemRef(int32 i.TableIndex - 1)
    // TODO: Make struct hashset type to avoid heap allocations when adding smaller number of rows.
    member private this.TryAddLoop(i, start, rows: ReadOnlySpan<'Row>, lookup: HashSet<'Row>): Result<TableIndexRange<'Row>, _> =
        if i < rows.Length then
            if lookup.Add rows.[i]
            then this.TryAddLoop(i + 1, start, rows, lookup)
            else Error i
        else Ok { Start = start; End = start + rows.Length - 1 }
    member this.TryAdd(rows: ReadOnlySpan<'Row>) = this.TryAddLoop(0, this.Count, rows, HashSet this.comparer)
    //member _.TryCopyRows({ Start = starti; End = endi }: TableIndexRange<'Row>, destination: Span<'Row>) =
    // TODO: Access underlying array of rows to get a Span containing each row.
    //member _.TryGetRows({ Start = starti; End = endi }: TableIndexRange<'Row>): ReadOnlySpan<'Row> =
