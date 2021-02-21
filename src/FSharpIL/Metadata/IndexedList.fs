namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type IndexedList<'T when 'T : equality> internal (owner: IndexOwner, items: ImmutableArray<'T>) =
    internal new (owner, items: ImmutableArray<'T>.Builder) = IndexedList<_>(owner, items.ToImmutable())

    member _.Count = items.Length
    member _.IsEmpty = items.IsEmpty
    member _.Item with get i = items.[i]
    member _.AsSpan() = items.AsSpan()
    member _.GetIndex i = SimpleIndex(owner, items.[i])
    member _.GetEnumerator() = items.GetEnumerator()

    interface IReadOnlyList<'T> with
        member this.Count = this.Count
        member this.Item with get i = this.[i]
        member _.GetEnumerator() = (items :> IEnumerable<_>).GetEnumerator()
        member _.GetEnumerator() = (items :> System.Collections.IEnumerable).GetEnumerator()

[<Sealed>]
type internal IndexedListBuilder<'T when 'T : equality and 'T :> IIndexValue> (owner: IndexOwner) =
    let lookup = HashSet<'T>()
    let items = ImmutableArray.CreateBuilder<'T>()

    member _.Count = items.Count
    member _.ToImmutable() = IndexedList(owner, items)

    member _.Add(value: 'T) =
        IndexOwner.checkOwner owner value
        if lookup.Add value
        then
            let i = items.Count
            items.Add value
            ValueSome i
        else ValueNone
