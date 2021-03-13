namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

[<NoComparison; ReferenceEquality>]
type OwnedMetadataTable<'Owner, 'T when 'Owner : equality and 'T : equality> =
    private
        { OwnedLookup: Dictionary<SimpleIndex<'Owner>, HashSet<SimpleIndex<'T>>>
          OwnedTable: MetadataTable<'T> }

    member this.Count = this.OwnedTable.Count
    member this.Indices = this.OwnedTable.Indices
    member this.Rows = this.OwnedTable.TableItems
    member this.Table = this.OwnedTable

    member this.TryGetRows owner =
        match this.OwnedLookup.TryGetValue owner with
        | (true, rows) -> ValueSome rows
        | (false, _) -> ValueNone

    member this.GetCount owner =
        match this.TryGetRows owner with
        | ValueSome rows -> uint32 rows.Count
        | ValueNone -> 0u

    interface IReadOnlyDictionary<SimpleIndex<'T>, int32> with
        member this.Count = this.Count
        member this.Item with get index = this.OwnedTable.Item index
        member this.Keys = this.Indices
        member this.Values = this.OwnedTable.TableLookup.Values
        member this.ContainsKey index = this.OwnedTable.TableLookup.ContainsKey index
        member this.TryGetValue(handle, index) = this.OwnedTable.TableLookup.TryGetValue(handle, &index)
        member this.GetEnumerator() = this.OwnedTable.TableLookup.GetEnumerator()
        member this.GetEnumerator() = this.OwnedTable.TableLookup.GetEnumerator() :> System.Collections.IEnumerator

/// <summary>Represents a table whose rows are conceptually owned by one row in another table.</summary>
[<Sealed>]
type OwnedMetadataTableBuilder<'Owner, 'T when 'Owner : equality and 'T : equality and 'Owner :> IIndexValue and 'T :> IIndexValue> =
    val private items: Dictionary<SimpleIndex<'Owner>, HashSet<'T>>
    val private owner: IndexOwner
    val mutable private count: int32

    new (owner) = { owner = owner; items = Dictionary<_, _>(); count = 0 }

    member this.Count = this.count

    /// <returns>An index to the value added to the table, or <c>ValueNone</c> if the value is a duplicate.</returns>
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    member this.Add(key, value) =
        IndexOwner.checkIndex this.owner key
        IndexOwner.checkOwner this.owner value
        let lookup =
            match this.items.TryGetValue key with
            | (true, existing) -> existing
            | (false, _) ->
                let empty = HashSet<'T>()
                this.items.[key] <- empty
                empty
        if lookup.Add value then
            this.count <- this.count + 1
            this.CreateIndex value |> ValueSome
        else ValueNone

    member internal this.ToImmutable() =
        let ownedLookup = Dictionary<_, HashSet<_>> this.items.Count
        let tableItems = ImmutableArray.CreateBuilder<_> this.count
        let tableLookup = Dictionary<_, int32> this.count
        for KeyValue(owner, items) in this.items do
            let items' = HashSet<_> items.Count
            for row in items do
                let i = tableItems.Count
                let index = this.CreateIndex row
                tableItems.Add row
                tableLookup.[index] <- i + 1
                if items'.Add index |> not then failwith "Unable to add row"
            ownedLookup.[owner] <- items'
        { OwnedLookup = ownedLookup
          OwnedTable = { TableItems = tableItems.ToImmutable(); TableLookup = tableLookup } }

    // NOTE: This is used in the Temp functions for owned count.
    member internal this.ToReadOnlyDictionary() = System.Collections.ObjectModel.ReadOnlyDictionary this.items

    member private this.CreateIndex item = SimpleIndex<'T>(this.owner, item)
