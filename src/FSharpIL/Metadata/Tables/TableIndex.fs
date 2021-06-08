namespace FSharpIL.Metadata.Tables

/// Marker interface used to indicate that a type represents a row in a metadata table.
type ITableRow = interface end

// NOTE: Since some tables are sorted, this may not correspond to the actual row numbers generated.
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type TableIndex<'Row when 'Row :> ITableRow> =
    internal { TableIndex: uint32 }
    member this.IsNull = this.TableIndex = 0u
    static member Zero = { TableIndex = 0u }

[<RequireQualifiedAccess>]
module internal TableIndex =
    [<GeneralizableValue>]
    let maximum<'Row> =
        // Maximum valid value is the maximum 3-byte unsigned integer value, since that is the maximum index that can be encoded
        // in a metadata token in a method body.
        { TableIndex = 0xFF_FF_FFu }
