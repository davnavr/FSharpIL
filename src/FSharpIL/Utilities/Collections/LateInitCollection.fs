namespace FSharpIL.Utilities.Collections

open System.Collections.Generic

type internal LateInitCollection<'Item, 'Inner when 'Inner :> ICollection<'Item> and 'Inner : (new: unit -> 'Inner) and 'Inner : null> = struct
    val mutable internal inner: 'Inner

    new (inner) = { inner = inner }

    member this.IsInitialized = not(isNull this.inner)
    member this.Count = if this.IsInitialized then this.inner.Count else 0
    member this.IsEmpty = this.Count = 0

    member this.Inner =
        if isNull this.inner then this.inner <- new 'Inner()
        this.inner

    member this.Add item = this.Inner.Add item

    member this.GetEnumerator() = if this.IsInitialized then this.Inner.GetEnumerator() else Seq.empty.GetEnumerator()
end

type internal LateInitDictionary<'Key, 'Value when 'Key : equality> = struct
    val mutable inner: LateInitCollection<KeyValuePair<'Key, 'Value>, Dictionary<'Key, 'Value>>

    new (inner) = { inner = inner }

    member this.Item
        with get key =
            if this.inner.IsInitialized
            then this.inner.inner.[key]
            else invalidOp "The dictionary was not yet initialized"
        and set key value = this.inner.Inner.[key] <- value

    member this.TryGetValue(key, value: outref<_>) =
        if this.IsInitialized
        then this.inner.inner.TryGetValue(key, &value)
        else false

    member this.Count = this.inner.Count
    member this.Inner = this.inner.Inner
    member this.IsInitialized = this.inner.IsInitialized

    member this.GetEnumerator() = this.inner.GetEnumerator()
end
