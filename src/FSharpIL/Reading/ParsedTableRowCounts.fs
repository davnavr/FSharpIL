namespace FSharpIL.Reading

open System.Collections.Generic

open FSharpIL.Metadata.Tables

[<Sealed>]
type ParsedTableRowCounts internal (lookup: IReadOnlyDictionary<ValidTableFlags, uint32>) =
    member _.Count = lookup.Count
    member _.GetEnumerator() = lookup.GetEnumerator() :> IEnumerator<_>
    interface IReadOnlyDictionary<ValidTableFlags, uint32> with
        member _.ContainsKey key = lookup.ContainsKey key
        member this.Count = this.Count
        member _.GetEnumerator() = lookup.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = lookup.GetEnumerator() :> System.Collections.IEnumerator
        member _.Item with get key = lookup.[key]
        member _.Keys = lookup.Keys
        member _.TryGetValue(key, value) = lookup.TryGetValue(key, &value)
        member _.Values = lookup.Values
    interface ITableRowCounts with member _.RowCount table = lookup.GetValueOrDefault table
