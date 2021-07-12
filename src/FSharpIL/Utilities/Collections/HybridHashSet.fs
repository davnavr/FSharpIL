﻿namespace rec FSharpIL.Utilities.Collections

open System.Collections.Generic
open System.Runtime.CompilerServices

type private HybridHashSetHelpers<'T> = struct
    interface IHybridCollectionMethods<'T, HashSet<'T>, HashSet<'T>.Enumerator> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.InitInner() = HashSet<'T>()
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.InnerEnumerator inner = inner.GetEnumerator()
        member _.EqualityComparer
            with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get() =
                EqualityComparer<'T>.Default :> IEqualityComparer<'T> // Don't forget to change this if a different comparer is used.
end

type internal HybridHashSet<'T when 'T : not struct and 'T : equality> = struct
    val mutable private inner: HybridCollection<'T, HashSet<'T>, HashSet<'T>.Enumerator, HybridHashSetHelpers<'T>>

    internal new (capacity) = { inner = HybridCollection.create capacity }

    member this.Count = this.inner.count
    member this.IsEmpty = this.inner.IsEmpty

    member this.Add item =
        match this.Count with
        | 0 ->
            this.inner.item0 <- item
            this.inner.count <- 1
            true
        | 1 when this.inner.ItemsEqual(this.inner.item0, item) -> false
        | 1 ->
            this.inner.item1 <- item
            this.inner.count <- 2
            true
        | 2 when
                this.inner.ItemsEqual(this.inner.item0, item)
                || this.inner.ItemsEqual(this.inner.item1, item)
            ->
                false
        | 2 ->
            this.inner.item2 <- item
            this.inner.count <- 3
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
            if result then this.inner.count <- this.inner.Count + 1
            result

    member this.Contains item = this.inner.Contains item

    member this.GetEnumerator() = this.inner.GetEnumerator()
end
