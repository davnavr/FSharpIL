namespace FSharpIL.Metadata.Tables

/// Marker interface used to indicate that a type represents a row in a metadata table.
type ITableRow = interface end

// NOTE: Since some tables are sorted, this may not correspond to the actual row numbers generated.
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type TableIndex<'Row when 'Row :> ITableRow> =
    internal { TableIndex: uint32 }
    member this.IsNull = this.TableIndex = 0u
    static member Zero = { TableIndex = 0u }
    static member op_Implicit { TableIndex = index } = index

[<RequireQualifiedAccess>]
module TableIndex =
    [<GeneralizableValue>]
    let internal maximum<'Row> =
        // Maximum valid value is the maximum 3-byte unsigned integer value, since that is the maximum index that can be encoded
        // in a metadata token in a method body.
        { TableIndex = 0xFF_FF_FFu }

    let ofIntUnsafe<'Row when 'Row :> ITableRow> index = { TableIndex = index }: TableIndex<'Row>
