namespace FSharpIL.Writing.Tables

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

type ITableIndexSizes = interface
    abstract SizeOf: table: ValidTableFlags -> uint32
end

type ITableBuilder<'Row when 'Row : struct and 'Row  :> ITableRow> = interface
    abstract Count: int32
    abstract Item: TableIndex<'Row> -> inref<'Row> with get
    abstract SerializeRow: HeapSizes * ITableIndexSizes * row: inref<'Row> * byref<ChunkedMemoryBuilder> -> unit
end

[<RequireQualifiedAccess>]
module TableBuilder =
    let inline count (builder: #ITableBuilder<_>) = builder.Count
    let inline get (builder: #ITableBuilder<_>) index = &builder.[index]
    let inline hasLargeIndices (builder: #ITableBuilder<_>) = count builder > 0xFFFF
    let inline indexSize (builder: #ITableBuilder<_>) = if hasLargeIndices builder then 4u else 2u

    let serialize (wr: byref<ChunkedMemoryBuilder>) hsizes tsizes (table: #ITableBuilder<_>) =
        for i = 1 to table.Count do
            table.SerializeRow(hsizes, tsizes, &table.[{ TableIndex = uint32 i }], &wr)

    let private writeStreamOffset (wr: byref<ChunkedMemoryBuilder>) size (offset: uint32) =
        match size with
        | 2u -> wr.WriteLE(uint16 offset)
        | 4u
        | _ -> wr.WriteLE offset

    let writeStringOffset (wr: byref<_>) (hsizes: HeapSizes) { StringOffset = offset } =
        writeStreamOffset &wr hsizes.StringSize offset

    let writeGuidOffset (wr: byref<_>) (hsizes: HeapSizes) { GuidIndex = index } =
        writeStreamOffset &wr hsizes.GuidSize index
