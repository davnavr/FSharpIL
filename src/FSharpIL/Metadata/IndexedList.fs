namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

[<NoEquality; NoComparison>]
type IndexedList<'Item when 'Item : equality and 'Item :> IIndexValue> =
    private
        { Owner: IndexOwner
          IndexedList: ImmutableArray<'Item>.Builder }

    member this.Count = this.IndexedList.Count
    member this.ToImmutable() = this.IndexedList.ToImmutable()

    interface IReadOnlyList<'Item> with
        member this.Count = this.IndexedList.Count
        member this.Item with get i = this.IndexedList.[i]
        member this.GetEnumerator() = this.IndexedList.GetEnumerator()
        member this.GetEnumerator() = this.IndexedList.GetEnumerator() :> System.Collections.IEnumerator

[<RequireQualifiedAccess>]
module IndexedList =
    [<RequiresExplicitTypeArguments>]
    let internal empty<'Item when 'Item : equality and 'Item :> IIndexValue> owner =
        { Owner = owner
          IndexedList = ImmutableArray.CreateBuilder<'Item>() }

    let length { IndexedList = items } = items.Count

    let add value list =
        list.Owner.CheckOwner value
        // TODO: Make lookup of duplicate items in IndexedList<_> more efficient.
        if list.IndexedList.Contains value
        then ValueNone
        else
            list.IndexedList.Add value
            SimpleIndex(list.Owner, value) |> ValueSome

    let inline toBlock (list: IndexedList<_>) = list.ToImmutable()
