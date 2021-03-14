namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

[<Interface>]
type IMetadataTable<'Index> =
    abstract Count: int32
    abstract Item: 'Index -> int32 with get

[<NoComparison; ReferenceEquality>]
type MetadataTable<'Row when 'Row : equality> =
    private
        { TableOwner: IndexOwner
          TableItems: ImmutableArray<'Row>
          TableLookup: IReadOnlyDictionary<SimpleIndex<'Row>, int32> }

    member this.Count = this.TableItems.Length
    // TODO: Create easier way to iterate indices in order.
    member this.Indices = Seq.map (fun item -> SimpleIndex<_>(this.TableOwner, item)) this.TableItems
    member this.Rows = this.TableItems
    member this.Item with get index = this.TableLookup.Item index

    interface IMetadataTable<SimpleIndex<'Row>> with
        member this.Count = this.Count
        member this.Item with get index = this.[index]

namespace global

open FSharpIL.Writing

[<AutoOpen>]
module MetadataTableExtensions =
    type FSharpIL.Metadata.IMetadataTable<'Index> with
        /// Gets a value indicating whether or not a simple index into this table takes up four or two bytes.
        member this.HasLargeIndices = this.Count >= 65536

        member this.IndexOf index = uint32 this.[index]

        member internal this.WriteSimpleIndex(i: uint32, writer: ChunkWriter) =
            if this.HasLargeIndices
            then writer.WriteU4 i
            else writer.WriteU2 i

        member internal this.WriteSimpleIndex(index, writer) =
            this.WriteSimpleIndex(this.IndexOf index, writer)
