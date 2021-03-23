namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

[<Sealed>]
type OwnedMetadataTable<'Owner, 'T when 'Owner : equality and 'T : equality> internal
    (
        lookup: Dictionary<RawIndex<'Owner>, ImmutableArray<RawIndex<'T>>>,
        items: ImmutableArray<'T>
    ) =
    /// Gets the sum of the number of items associated with each owning row.
    member _.Count = items.Length
    member _.Owners = lookup.Keys
    member _.Rows = items
    member _.Item with get (index: RawIndex<'T>) = items.[index.Value - 1]

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
    member _.Owners = items.Keys
    /// Gets the rows of this metadata table, unordered.
    member _.Rows = items.Values

    /// <returns>An index to the value added to the table, or <c>ValueNone</c> if the value is a duplicate.</returns>
    member _.TryAdd(key, value) =
        let lookup =
            match items.TryGetValue key with
            | (true, existing) -> existing
            | (false, _) ->
                let empty = HashSet<'T>()
                items.[key] <- empty
                empty
        if lookup.Add value then
            count <- count + 1 // TODO: Fix, this index will not be correct if TryAdd is called with different owners.
            ValueSome(RawIndex<'T> count)
        else ValueNone

    // TODO: Ensure that order of items matches owners in OwnedMetadataTable`2.
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
