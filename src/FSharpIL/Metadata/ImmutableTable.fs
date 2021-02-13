namespace FSharpIL.Metadata

open System
open System.Collections.Generic

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Writing

type ImmutableTable<'Index, 'T when 'Index :> IIndex and 'Index : equality> internal (table: IReadOnlyCollection<'T>, own: 'T -> 'Index) =
    let items = Array.zeroCreate<'T> table.Count
    let dict = Dictionary<'Index, uint32> table.Count

    do
        let mutable i = 0
        for item in table do
            Array.set items i item
            i <- i + 1 // TODO: Figure out if 1 refers to the first item for ALL tables.
            dict.Item <- own item, uint32 i

    member val Handles = dict.Keys :> IReadOnlyCollection<_>
    member val Items = Array.AsReadOnly items
    member val internal SimpleIndexSize = if items.Length < 65536 then 2 else 4

    member _.IndexOf handle = dict.Item handle

    member _.Count = items.Length

    member internal this.WriteSimpleIndex(i: uint32, writer: ChunkWriter) =
        if this.SimpleIndexSize = 2
        then writer.WriteU2 i
        else writer.WriteU4 i

    member internal this.WriteSimpleIndex(handle, writer) = this.WriteSimpleIndex(this.IndexOf handle, writer)

    interface IReadOnlyDictionary<'Index, uint32> with
        member this.Count = this.Count
        member this.Item with get handle = this.IndexOf handle
        member _.Keys = dict.Keys :> IEnumerable<_>
        member _.Values = dict.Values :> IEnumerable<_>
        member _.ContainsKey handle = dict.ContainsKey handle
        member _.TryGetValue(handle, index) = dict.TryGetValue(handle, &index)
        member _.GetEnumerator() = dict.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = dict.GetEnumerator() :> System.Collections.IEnumerator
