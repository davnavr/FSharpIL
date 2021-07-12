namespace rec FSharpIL.Utilities.Collections

open System
open System.Collections.Generic

type internal IHybridCollectionMethods<'Item, 'Inner, 'InnerEnumerator> = interface
    abstract EqualityComparer: IEqualityComparer<'Item>
    abstract InitInner: unit -> 'Inner
    abstract InnerEnumerator: 'Inner -> 'InnerEnumerator
end

[<Struct>]
type internal HybridCollection<'Item, 'Inner, 'InnerEnumerator, 'Methods
    when 'Inner :> ICollection<'Item>
    and 'Inner : null
    and 'InnerEnumerator :> IEnumerator<'Item>
    and 'InnerEnumerator : struct
    and 'Methods :> IHybridCollectionMethods<'Item, 'Inner, 'InnerEnumerator>
    and 'Methods : struct>
    =
    private
        { mutable inner: 'Inner
          [<DefaultValue(false)>] mutable count: int32
          [<DefaultValue(false)>] mutable item0: 'Item
          [<DefaultValue(false)>] mutable item1: 'Item
          [<DefaultValue(false)>] mutable item2: 'Item }

    member this.IsEmpty = this.count = 0

    member internal this.InitInner() =
        if this.count > 3 && isNull this.inner then
            this.inner <- Unchecked.defaultof<'Methods>.InitInner()

    member inline internal _.ItemsEqual(x, y) = Unchecked.defaultof<'Methods>.EqualityComparer.Equals(x, y)

    member this.Contains item =
        match this.count with
        | 0 -> false
        | 1 -> this.ItemsEqual(this.item0, item)
        | 2 -> this.ItemsEqual(this.item0, item) || this.ItemsEqual(this.item1, item)
        | 3 -> this.ItemsEqual(this.item0, item) || this.ItemsEqual(this.item1, item) || this.ItemsEqual(this.item2, item)
        | _ ->
            this.ItemsEqual(this.item0, item) ||
            this.ItemsEqual(this.item1, item) ||
            this.ItemsEqual(this.item2, item) ||
            (if isNull this.inner then false else this.inner.Contains item)

    member this.GetEnumerator() = HybridCollection.createValueEnumerator &this

[<RequireQualifiedAccess>]
module internal HybridCollection =
    type private EnumeratorState =
        | NotStarted = 0uy
        | First = 1uy
        | Second = 2uy
        | Third = 3uy
        | Rest = 4uy
        | ReachedEnd = 5uy

    let inline create capacity =
        { inner = if capacity > 3 then (^Inner : (new: int32 -> 'Inner) capacity) else null }: HybridCollection<_, 'Inner, _, _>

    [<Struct>]
    type Enumerator<'Item, 'InnerEnumerator when 'InnerEnumerator :> IEnumerator<'Item> and 'InnerEnumerator : struct> =
        private
            { mutable inner: 'InnerEnumerator
              mutable hasInner: bool
              mutable state: EnumeratorState
              item0: 'Item
              item1: 'Item
              item2: 'Item }
    
        member this.Current =
            match this.state with
            | EnumeratorState.NotStarted -> enumeratorNotStarted()
            | EnumeratorState.First -> this.item0
            | EnumeratorState.Second -> this.item1
            | EnumeratorState.Third -> this.item2
            | EnumeratorState.Rest -> this.inner.Current
            | EnumeratorState.ReachedEnd
            | _ -> enumeratorReachedEnd()
    
        member inline private this.ReachedEnd() =
            this.state <- EnumeratorState.ReachedEnd
            false

        member this.MoveNext() =
            match this.state with
            | EnumeratorState.NotStarted when Object.Equals(this.item0, Unchecked.defaultof<_>) -> this.ReachedEnd()
            | EnumeratorState.First when Object.Equals(this.item1, Unchecked.defaultof<_>) -> this.ReachedEnd()
            | EnumeratorState.Second when Object.Equals(this.item2, Unchecked.defaultof<_>) -> this.ReachedEnd()
            | EnumeratorState.NotStarted
            | EnumeratorState.First
            | EnumeratorState.Second
            | EnumeratorState.Third ->
                this.state <- this.state + EnumeratorState.First
                true
            | EnumeratorState.Rest when not this.hasInner -> this.ReachedEnd()
            | EnumeratorState.Rest ->
                let moved = this.inner.MoveNext()
                if not moved then this.state <- EnumeratorState.ReachedEnd
                moved
            | EnumeratorState.ReachedEnd
            | _ -> false
    
        interface IEnumerator<'Item> with
            member this.Current = this.Current
            member this.Current = this.Current :> obj
            member this.MoveNext() = this.MoveNext()
            member _.Reset() = enumeratorCannotReset()
            member _.Dispose() = ()

    let createValueEnumerator (collection: byref<HybridCollection<_, _, 'InnerEnumerator, 'Methods>>) =
        { inner =
            if isNull collection.inner
            then Unchecked.defaultof<_>
            else Unchecked.defaultof<'Methods>.InnerEnumerator collection.inner
          hasInner = not(isNull collection.inner)
          state = EnumeratorState.NotStarted
          item0 = collection.item0
          item1 = collection.item1
          item2 = collection.item2 }
