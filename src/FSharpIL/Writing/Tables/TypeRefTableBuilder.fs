namespace FSharpIL.Writing.Tables

open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables.Collections

[<Sealed>]
type TypeRefTableBuilder internal () =
    let rows = RowSet<TypeRefRow>()
    member _.TryAdd(row: inref<_>) =
        ()
    interface ITableBuilder<TypeRefRow> with
        member _.Count = rows.Count
        member _.Item with get i = &rows.[i]
        member _.Serialize(hsizes, tsizes, row, wr) =
            ()
