namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

[<Sealed>]
type IndexedList<'Tag, 'Item when 'Tag : equality and 'Item :> IIndexValue> internal (owner: IndexOwner, mapper: 'Item -> _) =
    let items = ImmutableArray.CreateBuilder<'Tag>()

    member _.Count = items.Count
    member _.ToImmutable() = items.ToImmutable()

    member _.Add(value: 'Item) =
        IndexOwner.checkOwner owner value
        let value' = mapper value
        // TODO: Make lookup of duplicate items in IndexedList<_> more efficient.
        if items.Contains value'
        then ValueNone
        else
            items.Add value'
            SimpleIndex(owner, value') |> ValueSome

    interface IReadOnlyList<'Tag> with
        member this.Count = this.Count
        member _.Item with get i = items.[i]
        member _.GetEnumerator() = items.GetEnumerator()
        member _.GetEnumerator() = items.GetEnumerator() :> System.Collections.IEnumerator
