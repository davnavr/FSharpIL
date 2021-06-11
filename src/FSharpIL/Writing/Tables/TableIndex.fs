namespace FSharpIL.Writing.Tables

open FSharpIL
open FSharpIL.Metadata.Tables

type ITableIndexSizes = interface
    abstract SizeOf: table: ValidTableFlags -> uint32
end

/// Contains functions for writing simple indices into tables (II.24.2.6).
[<RequireQualifiedAccess>]
module TableIndex =
    let [<Literal>] internal MaxSmallIndex = 0xFFFF

    let write
        (wr: byref<ChunkedMemoryBuilder>)
        (sizes: ITableIndexSizes)
        (table: ValidTableFlags)
        ({ TableIndex = i }: TableIndex<'Row>)
        =
        match sizes.SizeOf table with
        | 2u -> wr.WriteLE(uint16 i)
        | 4u
        | _ -> wr.WriteLE i
