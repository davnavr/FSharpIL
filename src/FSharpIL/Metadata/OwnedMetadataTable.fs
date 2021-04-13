namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

[<Sealed>]
type OwnedMetadataTable<'Owner, 'T when 'Owner : equality and 'T : equality> internal
    (
        itemLookup: Dictionary<RawIndex<'Owner>, ImmutableArray<RawIndex<'T>>>,
        ownerLookup: Dictionary<RawIndex<'T>, RawIndex<'Owner>>,
        items: ImmutableArray<'T>
    ) =
    /// Gets the sum of the number of items associated with each owning row.
    member _.Count = items.Length
    member _.Owners = itemLookup.Keys
    member _.Rows = items
    member _.Item with get (index: RawIndex<'T>) = &items.ItemRef(index.Value - 1)

    member _.TryGetRows owner =
        match itemLookup.TryGetValue owner with
        | (true, rows) -> ValueSome rows
        | (false, _) -> ValueNone

    member this.GetCount owner =
        match this.TryGetRows owner with
        | ValueSome items' -> uint32 items'.Length
        | ValueNone -> 0u

    member _.GetOwner index = ownerLookup.[index]

    interface IMetadataTable<'T> with
        member this.Count = this.Count
        member this.Item with get index = &this.[index]

/// <summary>Represents a table whose rows are conceptually owned by one row in another table.</summary>
[<Sealed>]
type OwnedMetadataTableBuilder<'Owner, 'T when 'Owner : equality and 'T : equality> internal() =
    let items = Dictionary<RawIndex<'Owner>, Dictionary<RawIndex<'T>, 'T>>()
    let mutable count = 0

    member _.Count = count
    member _.Owners = items.Keys
    /// Gets the rows of this metadata table, unordered.
    member _.Rows = items.Values

    /// <returns>An index to the value added to the table, or <c>ValueNone</c> if the value is a duplicate.</returns>
    member _.TryAdd(key, value) = // TODO: Make key and value inref.
        let lookup =
            match items.TryGetValue key with
            | (true, existing) -> existing
            | (false, _) ->
                let empty = Dictionary()
                items.[key] <- empty
                empty
        let i = RawIndex<'T>(count + 1)
        if lookup.TryAdd(i, value) then
            count <- i.Value
            ValueSome i
        else ValueNone

    // TODO: Ensure that order of items matches owners in OwnedMetadataTable`2.
    member internal _.ToImmutable() =
        let itemLookup = Dictionary items.Count
        let ownerLookup = Dictionary count
        let tableItems = ImmutableArray.CreateBuilder count
        for KeyValue(owner, items) in items do
            let items' = HashSet items.Count
            for KeyValue(irow, row) in items do
                tableItems.Add row
                ownerLookup.Add(RawIndex<'T> tableItems.Count, owner)
                if not(items'.Add irow) then sprintf "Duplicate row detected %O" irow |> invalidOp
            itemLookup.[owner] <- items'.ToImmutableArray()
        OwnedMetadataTable<'Owner, 'T>(itemLookup, ownerLookup, tableItems.ToImmutable())
