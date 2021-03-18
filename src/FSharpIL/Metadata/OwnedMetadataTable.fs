namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

[<Sealed>]
type OwnedMetadataTable<'Owner, 'T when 'Owner : equality and 'T : equality> internal
    (
        lookup: Dictionary<RawIndex<'Owner>, ImmutableArray<RawIndex<'T>>>,
        items: ImmutableArray<'T>
    ) =
    member _.Count = items.Length
    member _.Rows = items
    member this.Item with get (index: RawIndex<'T>) = items.[index.Value - 1]

    member _.TryGetRows owner =
        match lookup.TryGetValue owner with
        | (true, rows) -> ValueSome rows
        | (false, _) -> ValueNone

    member this.GetCount owner =
        match this.TryGetRows owner with
        | ValueSome rows -> uint32 rows.Length
        | ValueNone -> 0u

    interface IMetadataTable<'T> with
        member this.Count = this.Count
        member this.Item with get index = this.[index]

/// <summary>Represents a table whose rows are conceptually owned by one row in another table.</summary>
[<Sealed>]
type OwnedMetadataTableBuilder<'Owner, 'T when 'Owner : equality and 'T : equality> internal() =
    let items = Dictionary<RawIndex<'Owner>, HashSet<'T>>()
    let mutable count = 0

    member _.Count = count

    /// <returns>An index to the value added to the table, or <c>ValueNone</c> if the value is a duplicate.</returns>
    member _.Add(key, value) = // TODO: Rename to TryAdd
        let lookup =
            match items.TryGetValue key with
            | (true, existing) -> existing
            | (false, _) ->
                let empty = HashSet<'T>()
                items.[key] <- empty
                empty
        if lookup.Add value then
            count <- count + 1
            ValueSome(RawIndex<'T> count)
        else ValueNone

    member internal _.ToImmutable() =
        let lookup = Dictionary items.Count
        let tableItems = ImmutableArray.CreateBuilder<_> count
        for KeyValue(owner, items) in items do
            let items' = HashSet<_> items.Count
            for row in items do
                tableItems.Add row
                if not (items'.Add(RawIndex<'T> tableItems.Count)) then
                    failwith "Duplicate row detected"
            lookup.[owner] <- items'.ToImmutableArray()
        OwnedMetadataTable<'Owner, 'T>(lookup, tableItems.ToImmutable())
