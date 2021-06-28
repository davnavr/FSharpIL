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

    member this.GetEnumerator() = this.Inner.GetEnumerator()
end

type internal LateInitDictionary<'Key, 'Value when 'Key : equality> =
    LateInitCollection<KeyValuePair<'Key, 'Value>, Dictionary<'Key, 'Value>>
