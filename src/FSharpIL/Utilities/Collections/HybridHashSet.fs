namespace FSharpIL.Utilities.Collections

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
end
