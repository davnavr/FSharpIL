namespace FSharpIL.Reading

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

// TODO: Allow reading functions to end reading early by making the return value a voption.
type Reader<'Arg, 'State> = ('Arg -> uint64 -> 'State -> 'State) voption

type ErrorHandler<'State> = ReadState -> ReadError -> uint64 -> 'State -> 'State

[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type RvaAndSize = { Rva: uint32; Size: uint32 }

type ParsedCoffHeader = CoffHeader<uint16, uint16>
type ParsedStandardFields = StandardFields<PEImageKind, uint32, uint32 voption>
type ParsedNTSpecificFields = NTSpecificFields<uint64, uint32 * uint32, uint32, uint64, uint32>
type ParsedDataDirectories = ImmutableArray<RvaAndSize>
type ParsedSectionHeaders = ImmutableArray<SectionHeader<SectionLocation>>

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
    { /// Offset to the start of these stream from the beginning of the CLI metadata root.
      Offset: uint32
      /// The size of the stream in bytes, rounded up to a multiple of four.
      Size: uint32
      Name: ImmutableArray<byte> }

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
      Rows: ImmutableArray<uint32> }

    member this.StringIndexSize = if this.HeapSizes.HasFlag HeapSizes.String then 4 else 2
    member this.GuidIndexSize = if this.HeapSizes.HasFlag HeapSizes.Guid then 4 else 2

[<NoComparison; StructuralEquality>]
type ParsedModuleRow = // TODO: Allow Generation, EncId, and EncBaseId to be set to allow its usage in FSharpIL.Reading
    { Generation: uint16
      Name: RawIndex<string>
      Mvid: RawIndex<Guid>
      EncId: RawIndex<Guid>
      EncBaseId: RawIndex<Guid> }

// TODO: Rename this to something else.
[<NoComparison; NoEquality>]
type MetadataReader<'State> =
    { ReadLfanew: Reader<uint32, 'State>
      ReadCoffHeader: Reader<ParsedCoffHeader, 'State>
      ReadStandardFields: Reader<ParsedStandardFields, 'State>
      ReadNTSpecificFields: Reader<ParsedNTSpecificFields, 'State>
      ReadDataDirectories: Reader<ParsedDataDirectories, 'State>
      ReadSectionHeaders: Reader<ParsedSectionHeaders, 'State>
      ReadCliHeader: Reader<ParsedCliHeader, 'State>
      ReadMetadataRoot: Reader<ParsedMetadataRoot, 'State>
      ReadStreamHeader: (ParsedStreamHeader -> int32 -> uint64 -> 'State -> 'State) voption
      ReadMetadataTablesHeader: Reader<ParsedMetadataTablesHeader, 'State>
      ReadModuleTable: Reader<ParsedModuleRow, 'State> // TODO: Consider providing the GUIDs and strings read from both heaps, since it should be easy to read them.
      HandleError: ErrorHandler<'State> }

[<RequireQualifiedAccess>]
module MetadataReader =
    let private cont (_: uint64): 'State -> _ = id

    let inline internal read reader arg =
        match reader with
        | ValueSome reader' -> reader' arg
        | ValueNone -> cont

    // TODO: Mark most of these functions obsolete.
    let readLfanew { ReadLfanew = reader } lfanew = read reader lfanew
    let readCoffHeader { ReadCoffHeader = reader } header = read reader header
    let readStandardFields { ReadStandardFields = reader } fields = read reader fields
    let readNTSpecificFields { ReadNTSpecificFields = reader } fields = read reader fields
    let readDataDirectories { ReadDataDirectories = reader } directories = read reader directories
    let readSectionHeaders { ReadSectionHeaders = reader } headers = read reader headers
    let readCliHeader { ReadCliHeader = reader } header = read reader header
    let readMetadataRoot { ReadMetadataRoot = reader } root = read reader root
    let readStreamHeader { ReadStreamHeader = reader } header i offset =
        match reader with
        | ValueSome reader' -> reader' header i offset
        | ValueNone -> id
    let readMetadataTablesHeader { ReadMetadataTablesHeader = reader } header = read reader header

    let inline throwOnError (state: ReadState) error (offset: uint64) (_: 'State): 'State =
        ReadException(state, error, offset) |> raise

    [<GeneralizableValue>]
    let empty: MetadataReader<'State> =
        { ReadLfanew = ValueNone
          ReadCoffHeader = ValueNone
          ReadStandardFields = ValueNone
          ReadNTSpecificFields = ValueNone
          ReadDataDirectories = ValueNone
          ReadSectionHeaders = ValueNone
          ReadCliHeader = ValueNone
          ReadMetadataRoot = ValueNone
          ReadStreamHeader = ValueNone
          ReadMetadataTablesHeader = ValueNone
          ReadModuleTable = ValueNone
          HandleError = throwOnError }
