namespace FSharpIL.Writing.Tables.Collections

open FSharpIL.Utilities.Collections

open FSharpIL.Metadata.Tables

/// Contains the rows of a metadata table, and allows the addition of duplicate rows.
[<Sealed>]
type RowList<'Row when 'Row : equality and 'Row : struct and 'Row :> ITableRow> internal () =
    let rows = RefArrayList<'Row>()
    member _.Count = rows.Count
    member _.Add(row: inref<'Row>) =
        rows.Add &row |> ignore
        { TableIndex = uint32 rows.Count }
    member _.Item with get (i: TableIndex<'Row>) = &rows.[int32 i.TableIndex - 1]
