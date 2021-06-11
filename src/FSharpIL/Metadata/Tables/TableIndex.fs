namespace FSharpIL.Metadata.Tables

/// Marker interface used to indicate that a type represents a row in a metadata table.
type ITableRow = interface end

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type TableIndex<'Row when 'Row :> ITableRow> =
    internal { TableIndex: uint32 }
    member this.IsNull = this.TableIndex = 0u
    static member op_Implicit { TableIndex = index } = index

type ITableRowCounts = interface
    abstract RowCount: table: ValidTableFlags -> uint32
end

[<RequireQualifiedAccess>]
module TableIndex =
    let [<Literal>] internal MaxSmallIndex = 0xFFFFu

    let isLarge table (counts: #ITableRowCounts) = counts.RowCount table > MaxSmallIndex

    // TODO: Prevent index from exceeding maximum 3-byte integer.
    let ofIntUnsafe<'Row when 'Row :> ITableRow> index = { TableIndex = index }: TableIndex<'Row>
