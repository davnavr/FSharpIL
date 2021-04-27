namespace FSharpIL.Reading

open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

// TODO: Allow reading functions to end reading early by making the return value a voption.
type Reader<'Arg, 'State> = ('Arg -> uint64 -> 'State -> 'State) voption

type ErrorHandler<'State> = ReadState -> ReadError -> uint64 -> 'State -> 'State

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type RvaAndSize = { Rva: uint32; Size: uint32 }

type ParsedCoffHeader = CoffHeader<uint16, uint16>
type ParsedStandardFields = StandardFields<PEImageKind, uint32, uint32 voption>
type ParsedNTSpecificFields = NTSpecificFields<uint64, uint32 * uint32, uint32, uint64, uint32>
type ParsedDataDirectories = ImmutableArray<RvaAndSize>
type ParsedSectionHeaders = ImmutableArray<SectionHeader<SectionLocation>>

/// Represents a CLI header in a Portable Executable file that has been parsed (II.25.3.3).
type ParsedCliHeader =
    { MajorRuntimeVersion: uint16
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
type ParsedMetadataRoot =
    { MajorVersion: uint16
      MinorVersion: uint16
      Reserved: uint32
      Version: ImmutableArray<byte>
      Flags: uint16
      Streams: uint16 }

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
      HandleError: ErrorHandler<'State> }

[<RequireQualifiedAccess>]
module MetadataReader =
    let private cont (_: uint64): 'State -> _ = id

    let inline private read reader arg =
        match reader with
        | ValueSome reader' -> reader' arg
        | ValueNone -> cont

    let readLfanew { ReadLfanew = reader } lfanew = read reader lfanew
    let readCoffHeader { ReadCoffHeader = reader } header = read reader header
    let readStandardFields { ReadStandardFields = reader } fields = read reader fields
    let readNTSpecificFields { ReadNTSpecificFields = reader } fields = read reader fields
    let readDataDirectories { ReadDataDirectories = reader } directories = read reader directories
    let readSectionHeaders { ReadSectionHeaders = reader } headers = read reader headers
    let readCliHeader { ReadCliHeader = reader } header = read reader header

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
          HandleError = throwOnError }
