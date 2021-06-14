namespace FSharpIL.Writing.Tables.Collections

open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL.Utilities.Collections

open FSharpIL.Metadata.Tables

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type TableIndexRange<'Row when 'Row : struct and 'Row :> ITableRow> =
    internal { Start: int32; End: int32 }
    member this.Count = this.End - this.Start + 1
    member this.StartIndex = { TableIndex = uint32 this.Start }

[<Sealed>]
type RangedRowList<'Row when 'Row : struct and 'Row : equality and 'Row :> ITableRow> =
    val private rows: RefArrayList<'Row>
    val private comparer: IEqualityComparer<'Row>
    internal new (comparer) = { rows = RefArrayList(); comparer = comparer }
    internal new () = RangedRowList EqualityComparer.Default
    member this.Count = this.rows.Count
    member this.Item with get (i: TableIndex<'Row>) = &this.rows.[int32 i.TableIndex - 1]
    // TODO: Make struct hashset type to avoid heap allocations when adding smaller number of rows.
    member private this.TryAddLoop(i, start, rows: ImmutableArray<'Row>, lookup: HashSet<'Row>) = // TODO: Allow usage of ReadOnlySpan instead of ImmutableArray, figureo ut how to get inref to Span item.
        if i < rows.Length then
            if lookup.Add rows.[i] then
                this.rows.Add(&rows.ItemRef i) |> ignore
                this.TryAddLoop(i + 1, start, rows, lookup)
            else Error i
        else Result<TableIndexRange<'Row>, _>.Ok { Start = start; End = start + rows.Length - 1 }
    member this.TryAdd rows = this.TryAddLoop(0, this.Count, rows, HashSet this.comparer)
    //member _.TryCopyRows({ Start = starti; End = endi }: TableIndexRange<'Row>, destination: Span<'Row>) =
    // TODO: Access underlying array of rows to get a Span containing each row.
    //member _.TryGetRows({ Start = starti; End = endi }: TableIndexRange<'Row>): ReadOnlySpan<'Row> =
