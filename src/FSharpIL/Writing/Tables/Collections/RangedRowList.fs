namespace FSharpIL.Writing.Tables.Collections

open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL.Utilities.Collections

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type TableIndexRange<'Row when 'Row : struct and 'Row :> ITableRow> =
    internal { Start: int32; End: int32 }
    member this.Count = this.End - this.Start + 1
    member this.StartIndex = { TableIndex = uint32 this.Start }

type IRowRangeValidator<'Row when 'Row : struct and 'Row :> ITableRow> = interface
    abstract Validate: row: inref<'Row> -> IValidationError option
    abstract Duplicate: row: inref<'Row> -> IValidationError
end

[<Sealed>]
type RangedRowList<'Row, 'Validator
    when 'Row :> ITableRow
    and 'Row : struct
    and 'Row : equality
    and 'Validator :> IRowRangeValidator<'Row>
    and 'Validator : struct>
    =
    val private rows: RefArrayList<'Row>
    val private comparer: IEqualityComparer<'Row>
    internal new (comparer) = { rows = RefArrayList(); comparer = comparer }
    internal new () = RangedRowList EqualityComparer.Default
    member this.Count = this.rows.Count
    member this.Item with get (i: TableIndex<'Row>) = &this.rows.[int32 i.TableIndex - 1]
    member inline internal _.Validator = Unchecked.defaultof<'Validator>

    // TODO: Allow usage of ReadOnlySpan instead of ImmutableArray, figure out how to get inref to Span item.
    member private this.TryAddLoop(i, start, rows: ImmutableArray<'Row>, lookup: HashSet<'Row>) =
        if i < rows.Length then
            let row = &rows.ItemRef i
            if lookup.Add row then
                match this.Validator.Validate &row with
                | None ->
                    this.rows.Add &row |> ignore
                    this.TryAddLoop(i + 1, start, rows, lookup)
                | Some err -> Error err
            else Error(this.Validator.Duplicate &row)
        else Result<TableIndexRange<'Row>, _>.Ok { Start = start; End = start + rows.Length - 1 }

    member this.TryAdd(rows: ImmutableArray<_>) =
        let start = this.Count
        match rows.Length with
        | 0 -> failwith "TODO: What to do if empty rows array is added?"
        | 1 ->
            this.rows.Add(&rows.ItemRef 0) |> ignore
            Ok { Start = start; End = start }
        | _ -> this.TryAddLoop(0, start, rows, HashSet this.comparer)
    //member _.TryCopyRows({ Start = starti; End = endi }: TableIndexRange<'Row>, destination: Span<'Row>) =
    // TODO: Access underlying array of rows to get a Span containing each row.
    //member _.TryGetRows({ Start = starti; End = endi }: TableIndexRange<'Row>): ReadOnlySpan<'Row> =
