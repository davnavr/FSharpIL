namespace rec FSharpIL.Utilities.Collections

open System.Collections.Generic

type internal HybridHashSet<'T when 'T : not struct and 'T : equality> = struct
    val mutable private inner: HashSet<'T>
    [<DefaultValue>] val mutable private count: int32
    [<DefaultValue(false)>] val mutable private item0: 'T
    [<DefaultValue(false)>] val mutable private item1: 'T
    [<DefaultValue(false)>] val mutable private item2: 'T

    internal new (capacity) = { inner = if capacity > 3 then HashSet<'T>(capacity - 3) else Unchecked.defaultof<_> }

    member this.Count = this.count
    member this.IsEmpty = this.count = 0

    member private this.InitInnerSet() =
        if this.count > 3 && isNull this.inner then
            this.inner <- HashSet<'T>()

    member inline private this.ItemsEqual(x, y) = this.inner.Comparer.Equals(x, y)

    member this.Add item =
        match this.count with
        | 0 ->
            this.item0 <- item
            this.count <- 1
            true
        | 1 when this.ItemsEqual(this.item0, item) -> false
        | 1 ->
            this.item1 <- item
            this.count <- 2
            true
        | 2 when this.ItemsEqual(this.item0, item) || this.ItemsEqual(this.item1, item) -> false
        | 2 ->
            this.item2 <- item
            this.count <- 3
            true
        | _ when this.ItemsEqual(this.item0, item) || this.ItemsEqual(this.item1, item) || this.ItemsEqual(this.item2, item) ->
            false
        | _ ->
            this.InitInnerSet()
            let result = this.inner.Add item
            if result then this.count <- this.count + 1
            result

    member this.Contains item =
        match this.count with
        | 0 -> false
        | 1 -> this.ItemsEqual(this.item0, item)
        | 2 -> this.ItemsEqual(this.item0, item) || this.ItemsEqual(this.item1, item)
        | _ ->
            this.ItemsEqual(this.item0, item) ||
            this.ItemsEqual(this.item1, item) ||
            this.ItemsEqual(this.item2, item) ||
            (if isNull this.inner then false else this.inner.Contains item)

    member this.GetEnumerator() =
        { Inner = if isNull this.inner then Unchecked.defaultof<_> else this.inner.GetEnumerator()
          HasInner = not(isNull this.inner)
          State = EnumeratorState.NotStarted
          Item0 = this.item0
          Item1 = this.item1
          Item2 = this.item2 }

    interface IReadOnlyCollection<'T> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
end

type private EnumeratorState =
    | NotStarted = 0uy
    | First = 1uy
    | Second = 2uy
    | Third = 3uy
    | Rest = 4uy
    | ReachedEnd = 5uy

[<Struct>]
type HybridHashSetEnumerator<'T when 'T : not struct and 'T : equality> =
    private
        { mutable Inner: HashSet<'T>.Enumerator
          mutable HasInner: bool
          mutable State: EnumeratorState
          Item0: 'T
          Item1: 'T
          Item2: 'T }

    member this.Current =
        match this.State with
        | EnumeratorState.NotStarted -> enumeratorNotStarted()
        | EnumeratorState.First -> this.Item0
        | EnumeratorState.Second -> this.Item1
        | EnumeratorState.Third -> this.Item2
        | EnumeratorState.Rest -> this.Inner.Current
        | EnumeratorState.ReachedEnd
        | _ -> enumeratorReachedEnd()

    member this.MoveNext() =
        match this.State with
        | EnumeratorState.NotStarted
        | EnumeratorState.First
        | EnumeratorState.Second
        | EnumeratorState.Third ->
            this.State <- this.State + EnumeratorState.First
            true
        | EnumeratorState.Rest when not this.HasInner ->
            this.State <- EnumeratorState.ReachedEnd
            false
        | EnumeratorState.Rest ->
            let moved = this.Inner.MoveNext()
            if not moved then this.State <- EnumeratorState.ReachedEnd
            moved
        | EnumeratorState.ReachedEnd
        | _ -> false

    interface IEnumerator<'T> with
        member this.Current = this.Current
        member this.Current = this.Current :> obj
        member this.MoveNext() = this.MoveNext()
        member _.Reset() = enumeratorCannotReset()
        member _.Dispose() = ()
