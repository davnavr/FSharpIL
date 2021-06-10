namespace FSharpIL.Writing.Tables

open FSharpIL
open FSharpIL.Metadata.Tables

type TableIndexSizes = System.Collections.Generic.IReadOnlyDictionary<ValidTableFlags, uint32>

type ITableBuilder<'Row when 'Row : struct and 'Row  :> ITableRow> = interface
    abstract Count: int32
    abstract Item: TableIndex<'Row> -> inref<'Row> with get
    abstract Serialize: HeapSizes * TableIndexSizes * row: inref<'Row> * byref<ChunkedMemoryBuilder> -> unit
end
