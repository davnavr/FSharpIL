namespace FSharpIL.Utilities.Collections

open System.Collections.Generic
open System.Runtime.CompilerServices

type private HybridHashMapHelpers<'Key, 'Value when 'Key : equality> = struct
    interface IHybridCollectionMethods<KeyValuePair<'Key, 'Value>, Dictionary<'Key, 'Value>, Dictionary<'Key, 'Value>.Enumerator> with
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.InitInner() = Dictionary<'Key, 'Value>()
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.InnerEnumerator inner = inner.GetEnumerator()
        [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
        member _.ItemsEqual(inner, x, y) = inner.Comparer.Equals(x.Key, y.Key)
end

type internal HybridHashMap<'Key, 'Value when 'Key : not struct and 'Key : equality> = struct
    val mutable private inner:
        HybridCollection<KeyValuePair<'Key, 'Value>,
                         Dictionary<'Key, 'Value>,
                         Dictionary<'Key, 'Value>.Enumerator,
                         HybridHashMapHelpers<'Key, 'Value>>

    internal new (capacity) = { inner = HybridCollection.create capacity }

    member this.Count = this.inner.Count
    member this.IsEmpty = this.inner.IsEmpty

    member this.Item
        with set(key, value) =
            match this.Count with
            | 0 ->
                this.inner.item0 <- KeyValuePair(key, value)
                this.inner.Count <- 1
            | 1 when this.inner.inner.Comparer.Equals(this.inner.item0.Key, key) -> ()

    member this.GetEnumerator() = this.inner.GetEnumerator()
end
