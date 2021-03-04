namespace FSharpIL.Metadata

open System.Collections.Generic

type OwnedTableBuilderEnumerator<'Owner, 'T when 'Owner : equality and 'T : equality and 'Owner :> IIndexValue and 'T :> IIndexValue> =
    struct
        val mutable private keyEnumerator: Dictionary<SimpleIndex<'Owner>, HashSet<'T>>.Enumerator
        val mutable private valueEnumerator: HashSet<'T>.Enumerator
        val mutable private moved: bool
        new (items: Dictionary<SimpleIndex<'Owner>, HashSet<'T>>) =
            { keyEnumerator = items.GetEnumerator()
              valueEnumerator = Unchecked.defaultof<_>
              moved = false }

        member this.Current = this.valueEnumerator.Current

        member this.MoveNext() =
            if not this.moved then
                if this.keyEnumerator.MoveNext() then
                    this.valueEnumerator <- this.keyEnumerator.Current.Value.GetEnumerator()
                    this.moved <- this.valueEnumerator.MoveNext()
                    this.moved
                else false
            else
                this.moved <- this.valueEnumerator.MoveNext()
                this.moved

        interface System.IDisposable with member _.Dispose() = ()
        interface IEnumerator<'T> with
            member this.Current = this.Current
            member this.Current = this.Current :> obj
            member this.MoveNext() = this.MoveNext()
            member this.Reset() =
                (this.keyEnumerator :> IEnumerator<_>).Reset()
                this.valueEnumerator <- Unchecked.defaultof<_>
                this.moved <- false
    end

/// <summary>Represents a table whose rows are conceptually owned by one row in another table.</summary>
[<Sealed>]
type OwnedTableBuilder<'Owner, 'T when 'Owner : equality and 'T : equality and 'Owner :> IIndexValue and 'T :> IIndexValue> =
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
        if lookup.Add value
        then
            this.count <- this.count + 1
            SimpleIndex(this.owner, value) |> ValueSome
        else ValueNone

    member this.GetEnumerator() = new OwnedTableBuilderEnumerator<'Owner, 'T>(this.items)

    member internal this.ToReadOnlyDictionary() = System.Collections.ObjectModel.ReadOnlyDictionary this.items

    interface IReadOnlyCollection<'T> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
