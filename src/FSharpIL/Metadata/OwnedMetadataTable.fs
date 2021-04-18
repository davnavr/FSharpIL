namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

[<Sealed>]
type OwnedMetadataTable<'Owner, 'T when 'Owner : equality and 'T : equality> internal
    (
        itemLookup: Dictionary<RawIndex<'Owner>, ImmutableArray<RawIndex<'T>>>,
        ownerLookup: Dictionary<RawIndex<'T>, RawIndex<'Owner>>,
        indexMap: Dictionary<RawIndex<'T>, int32>,
        items: ImmutableArray<'T>
    ) =
    /// Gets the sum of the number of items associated with each owning row.
    member _.Count = items.Length
    member _.Owners = itemLookup.Keys
    member _.Rows = items
    member _.Item with get(index: RawIndex<'T>) = &items.ItemRef indexMap.[index]

    member _.TryGetRows owner =
        match itemLookup.TryGetValue owner with
        | (true, rows) -> ValueSome rows
        | (false, _) -> ValueNone

    member this.GetCount owner =
        match this.TryGetRows owner with
        | ValueSome items' -> uint32 items'.Length
        | ValueNone -> 0u

    member _.GetOwner index = ownerLookup.[index]

    member _.GetRowIndex(index: RawIndex<'T>) = uint32 indexMap.[index] + 1u

    interface IMetadataTable<'T> with
        member this.Count = this.Count
        member this.Item with get index = &this.[index]

/// <summary>Represents a table whose rows are conceptually owned by one row in another table.</summary>
[<Sealed>]
type OwnedMetadataTableBuilder<'Owner, 'T when 'Owner : equality and 'T : equality> internal() =
    let items = Dictionary<RawIndex<'Owner>, Dictionary<'T, int32>>()
    let mutable count = 0

    member _.Count = count
    member _.Owners = items.Keys :> IReadOnlyCollection<_>
    /// Gets the rows of this metadata table, unordered.
    member _.Rows = Seq.collect (fun (lookup: Dictionary<_, _>) -> lookup.Keys ) items.Values

    member _.TryAdd(key, value: inref<'T>) =
        let lookup =
            match items.TryGetValue key with
            | (true, existing) -> existing
            | (false, _) ->
                let empty = Dictionary()
                items.[key] <- empty
                empty
        if lookup.TryAdd(value, count) then
            count <- count + 1
            ValueSome(RawIndex<'T> count)
        else ValueNone

    // TODO: Ensure that order of items matches owners in OwnedMetadataTable`2.
    member internal _.ToImmutable() =
        let itemLookup = Dictionary items.Count
        let indexMap = Dictionary items.Count
        let ownerLookup = Dictionary count
        let mutable rows, itable = Array.zeroCreate<'T> count, 0
        for KeyValue(owner, rows') in items do
            let start = itable
            let mutable indices = Array.zeroCreate<RawIndex<'T>> rows'.Count
            for KeyValue(row, irow) in rows' do
                let irow' = RawIndex<'T>(irow + 1)
                indices.[itable - start] <- irow'
                rows.[itable] <- row
                indexMap.[irow'] <- itable
                itable <- itable + 1
                ownerLookup.[irow'] <- owner
            itemLookup.[owner] <- Unsafe.As &indices
        OwnedMetadataTable<'Owner, 'T>(itemLookup, ownerLookup, indexMap, Unsafe.As &rows)
