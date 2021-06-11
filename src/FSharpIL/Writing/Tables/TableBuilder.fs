namespace FSharpIL.Writing.Tables

open FSharpIL
open FSharpIL.Metadata.Tables

type ITableBuilder<'Row when 'Row : struct and 'Row  :> ITableRow> = interface
    abstract Count: int32
    abstract Item: TableIndex<'Row> -> inref<'Row> with get
    abstract SerializeRow: HeapSizes * ITableRowCounts * row: inref<'Row> * byref<ChunkedMemoryBuilder> -> unit
end

[<RequireQualifiedAccess>]
module TableBuilder =
    let inline count (builder: #ITableBuilder<_>) = builder.Count
    let inline get (builder: #ITableBuilder<_>) index = &builder.[index]

    let serialize (wr: byref<ChunkedMemoryBuilder>) hsizes tsizes (table: #ITableBuilder<_>) =
        for i = 1 to table.Count do
            table.SerializeRow(hsizes, tsizes, &table.[{ TableIndex = uint32 i }], &wr)
