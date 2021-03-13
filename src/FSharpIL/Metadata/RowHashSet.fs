namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

/// Contains the rows of a metadata table, and prevents the addition of duplicate rows.
[<System.Runtime.CompilerServices.IsReadOnly>]
type RowHashSet<'T when 'T : equality> = struct
    val private items: ImmutableDictionary<'T, int32>.Builder
    val private owner: IndexOwner
    internal new (owner) = { items = ImmutableDictionary.CreateBuilder<_, _>(); owner = owner }
    member this.Count = this.items.Count
    member this.TryAdd item =
        if this.items.TryAdd(item, this.Count)
        then SimpleIndex<'T>(this.owner, item) |> ValueSome
        else ValueNone
    member this.GetEnumerator() = this.items.Keys.GetEnumerator()
    member this.ToArray() =
        let items' = Array.zeroCreate this.Count
        for KeyValue(key, i) in this.items do
            items'.[i] <- key
        items'
    member internal this.ToImmutableDictionary() = this.items.ToImmutable()

    interface IReadOnlyCollection<'T> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator()
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
end
