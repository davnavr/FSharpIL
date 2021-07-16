namespace FSharpIL.Metadata.Tables

open FSharpIL.Utilities

/// <summary>
/// Specifies the sizes of offsets into the <c>#Strings</c>, <c>#GUID</c>, and <c>#Blob</c> streams (II.24.2.6).
/// </summary>
[<System.Flags>]
type HeapSizes =
    | None = 0uy
    /// <summary>Specifies that offsets into the <c>#Strings</c> stream should be 4 bytes wide.</summary>
    | String = 0x1uy
    /// <summary>Specifies that offsets into the <c>#GUID</c> stream should be 4 bytes wide.</summary>
    | Guid = 0x2uy
    /// <summary>Specifies that offsets into the <c>#Blob</c> stream should be 4 bytes wide.</summary>
    | Blob = 0x4uy

[<AutoOpen>]
module HeapSizes =
    let inline private isLarge (flags: HeapSizes) heap = if Flags.set heap flags then 4u else 2u
    type HeapSizes with
        member this.StringSize = isLarge this HeapSizes.String
        member this.GuidSize = isLarge this HeapSizes.Guid
        member this.BlobSize = isLarge this HeapSizes.Blob

/// <summary>
/// Represents the fields of the <c>#~</c> stream, which contain information about the metadata tables (II.24.2.6).
/// </summary>
type TablesHeader<'RowCounts when 'RowCounts :> ITableRowCounts> =
    { Reserved1: uint32
      MajorVersion: uint8
      MinorVersion: uint8
      HeapSizes: HeapSizes
      Reserved2: uint8
      /// Specifies which metadata tables are present.
      Valid: ValidTableFlags
      Sorted: ValidTableFlags
      /// Specifies the number of rows in each present metadata table.
      Rows: 'RowCounts }
