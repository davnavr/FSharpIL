namespace FSharpIL.Metadata.Heaps

open System.Collections.Generic
open System.Runtime.CompilerServices

open FSharpIL.Writing

type internal IHeap =
    abstract ByteLength: uint32

[<AutoOpen>]
module internal HeapExtensions =
    [<Literal>]
    let private MaxSmallIndex = 0xFFFFu

    let (|LargeIndex|SmallIndex|) (heap: #IHeap) =
        if heap.ByteLength > MaxSmallIndex
        then LargeIndex
        else SmallIndex

    type IHeap with
        member this.IndexSize =
            match this with
            | LargeIndex -> 4
            | SmallIndex -> 2

    type LookupHeap<'Heap, 'Item when 'Heap :> IReadOnlyDictionary<'Item, uint32> and 'Heap :> IHeap> = 'Heap

    [<Extension>]
    [<AbstractClass; Sealed>]
    type SpecificHeapExtensions =
        [<Extension>]
        static member WriteIndex(heap: LookupHeap<_, _>, item, writer: ChunkWriter) =
            let i = heap.Item item
            match heap with
            | LargeIndex -> writer.WriteU4 i
            | SmallIndex -> writer.WriteU2 i

        [<Extension>]
        static member WriteStringIndex(heap: LookupHeap<'Heap, string>, name: 'T, writer) = heap.WriteIndex(name.ToString(), writer)
        [<Extension>]
        static member WriteZero(heap: LookupHeap<'Heap, System.Guid>, writer) = heap.WriteIndex(System.Guid.Empty, writer)
