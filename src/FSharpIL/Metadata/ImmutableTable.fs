namespace FSharpIL.Metadata

open System
open System.Collections.Generic

open Microsoft.FSharp.Core.Operators.Checked

// TODO: How to remove equality constraint?
type ImmutableTable<'T when 'T : equality> internal (table: IReadOnlyCollection<'T>, own: 'T -> Handle<'T>) =
    let items = Array.zeroCreate<'T> table.Count
    let dict = Dictionary<Handle<'T>, uint32> table.Count

    do
        let mutable i = 0
        for item in table do
            Array.set items i item
            dict.Item <- own item, uint32 i
            i <- i + 1

    member val Handles = dict.Keys :> IReadOnlyCollection<_>
    member val Items = Array.AsReadOnly items

    member _.IndexOf handle = dict.Item handle

    member val Count = table.Count

    interface IReadOnlyDictionary<Handle<'T>, uint32> with
        member _.Count = dict.Count
        member this.Item with get handle = this.IndexOf handle
        member _.Keys = dict.Keys :> IEnumerable<_>
        member _.Values = dict.Values :> IEnumerable<_>
        member _.ContainsKey handle = dict.ContainsKey handle
        member _.TryGetValue(handle, index) = dict.TryGetValue(handle, &index)
        member _.GetEnumerator() = dict.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = dict.GetEnumerator() :> System.Collections.IEnumerator
