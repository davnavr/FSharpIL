namespace FSharpIL.Reading

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

/// Represents a CLI header in a Portable Executable file that has been parsed (II.25.3.3).
[<NoComparison; StructuralEquality>]
type ParsedCliHeader =
    { Size: uint32
      MajorRuntimeVersion: uint16
      MinorRuntimeVersion: uint16
      MetaData: RvaAndSize
      Flags: CorFlags
      EntryPointToken: uint32
      Resources: RvaAndSize
      StrongNameSignature: uint64
      CodeManagerTable: uint64
      VTableFixups: RvaAndSize
      ExportAddressTableJumps: uint64
      ManagedNativeHeader: uint64 }

/// (II.24.2.1)
[<NoComparison; StructuralEquality>]
type ParsedMetadataRoot =
    { MajorVersion: uint16
      MinorVersion: uint16
      Reserved: uint32
      Version: MetadataVersion
      Flags: uint16
      Streams: uint16 }

/// (II.24.2.2)
[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type ParsedStreamHeader =
    { /// Offset to the start of this stream from the beginning of the CLI metadata root.
      Offset: uint32
      /// The size of the stream in bytes, rounded up to a multiple of four.
      Size: uint32
      Name: ImmutableArray<byte> }

type MetadataTableCounts = System.Collections.Generic.IReadOnlyDictionary<MetadataTableFlags, uint32>

/// (II.24.2.6)
[<NoComparison; StructuralEquality>]
type ParsedMetadataTablesHeader =
    { Reserved1: uint32
      MajorVersion: uint8
      MinorVersion: uint8
      HeapSizes: HeapSizes
      Reserved2: uint8
      /// Specifies which metadata tables are present.
      Valid: MetadataTableFlags
      Sorted: MetadataTableFlags
      /// Specifies the number of rows in each present metadata table.
      Rows: MetadataTableCounts }
