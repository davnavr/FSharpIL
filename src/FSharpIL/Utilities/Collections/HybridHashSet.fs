namespace rec FSharpIL.Utilities.Collections

open System.Collections.Generic
open System.Runtime.CompilerServices

type private HybridHashSetHelpers<'T> = struct
    interface IHybridCollectionMethods<'T, HashSet<'T>, HashSet<'T>.Enumerator> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.InitInner() = HashSet<'T>()
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.InnerEnumerator inner = inner.GetEnumerator()
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.ItemsEqual(inner, x, y) = inner.Comparer.Equals(x, y)
end

type internal HybridHashSet<'T when 'T : not struct and 'T : equality> = struct
    val mutable private inner: HybridCollection<'T, HashSet<'T>, HashSet<'T>.Enumerator, HybridHashSetHelpers<'T>>

    internal new (capacity) = { inner = HybridCollection.create capacity }

    member this.Count = this.inner.Count
    member this.IsEmpty = this.inner.IsEmpty

    member this.Add item =
        match this.Count with
        | 0 ->
            this.inner.item0 <- item
            this.inner.Count <- 1
            true
        | 1 when this.inner.ItemsEqual(this.inner.item0, item) -> false
        | 1 ->
            this.inner.item1 <- item
            this.inner.Count <- 2
            true
        | 2 when
                this.inner.ItemsEqual(this.inner.item0, item)
                || this.inner.ItemsEqual(this.inner.item1, item)
            ->
                false
        | 2 ->
            this.inner.item2 <- item
            this.inner.Count <- 3
            true
        | _ when
                this.inner.ItemsEqual(this.inner.item0, item)
                || this.inner.ItemsEqual(this.inner.item1, item)
                || this.inner.ItemsEqual(this.inner.item2, item)
            ->
                false
        | _ ->
            this.inner.InitInner()
            let result = this.inner.inner.Add item
            if result then this.inner.Count <- this.inner.Count + 1
            result

    member this.Contains item = this.inner.Contains item

    member this.GetEnumerator() = this.inner.GetEnumerator()
end
