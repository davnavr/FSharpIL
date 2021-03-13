namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Writing

[<NoComparison; ReferenceEquality>]
type MetadataTable<'T when 'T : equality> =
    private
        { TableItems: ImmutableArray<'T>
          TableLookup: IReadOnlyDictionary<SimpleIndex<'T>, int32> }

    member this.Count = this.TableItems.Length
    member this.Indices = this.TableLookup.Keys
    member this.Rows = this.TableItems
    /// Gets a value indicating whether or not a simple index into this table takes up four or two bytes.
    member this.HasLargeIndices = this.Count >= 65536
    member this.Item with get index = this.TableLookup.Item index

    member this.IndexOf index = uint32 this.[index]

    member internal this.WriteSimpleIndex(i: uint32, writer: ChunkWriter) =
        if this.HasLargeIndices
        then writer.WriteU4 i
        else writer.WriteU2 i

    member internal this.WriteSimpleIndex(index, writer) = this.WriteSimpleIndex(uint32 this.[index], writer)

    interface IReadOnlyDictionary<SimpleIndex<'T>, int32> with
        member this.Count = this.Count
        member this.Item with get index = this.Item index
        member this.Keys = this.Indices
        member this.Values = this.TableLookup.Values
        member this.ContainsKey index = this.TableLookup.ContainsKey index
        member this.TryGetValue(handle, index) = this.TableLookup.TryGetValue(handle, &index)
        member this.GetEnumerator() = this.TableLookup.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.TableLookup.GetEnumerator() :> System.Collections.IEnumerator
