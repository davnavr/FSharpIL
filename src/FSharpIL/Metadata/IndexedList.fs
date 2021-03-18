namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

[<System.Obsolete>]
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type IndexedList<'T when 'T : equality> internal (items: ImmutableArray<'T>) =
    member _.Count = items.Length
    member _.IsEmpty = items.IsEmpty
    member _.Item with get i = items.[i]
    member _.AsSpan() = items.AsSpan()
    /// <exception cref="T:System.ArgumentOutOfRangeException">Thrown when the index is negative or the list does not contain enough elements.</exception>
    member _.GetIndex index = failwith "Do not use this"
    member _.GetEnumerator() = items.GetEnumerator()

    interface IReadOnlyList<'T> with
        member this.Count = this.Count
        member this.Item with get i = this.[i]
        member _.GetEnumerator() = (items :> IEnumerable<_>).GetEnumerator()
        member _.GetEnumerator() = (items :> System.Collections.IEnumerable).GetEnumerator()

    static member Empty = IndexedList ImmutableArray<'T>.Empty

[<System.Obsolete>]
[<Sealed>]
type internal IndexedListBuilder<'T when 'T : equality> () =
    let lookup = HashSet<'T>()
    let items = ImmutableArray.CreateBuilder<'T>()

    member _.Count = items.Count
    member _.ToImmutable() = IndexedList(items.ToImmutable())

    member _.Add(value: 'T) =
        if lookup.Add value
        then
            let i = items.Count
            items.Add value
            ValueSome i
        else ValueNone
