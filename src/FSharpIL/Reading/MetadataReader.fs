namespace FSharpIL.Reading

open System.Collections.Immutable

open FSharpIL.PortableExecutable

// TODO: Allow reading functions to end reading early by making the return value a voption.
type Reader<'Arg, 'State> = ('Arg -> uint64 -> 'State -> 'State) voption

type ErrorHandler<'State> = ReadState -> ReadError -> uint64 -> 'State -> 'State

type ParsedCoffHeader = CoffHeader<uint16, uint16>
type ParsedStandardFields = StandardFields<PEImageKind, uint32, uint32 voption>
type ParsedNTSpecificFields = NTSpecificFields<uint64, uint32 * uint32, uint32, uint64, uint32>
type ParsedDataDirectories = ImmutableArray<RvaAndSize>
type ParsedSectionHeaders = ImmutableArray<SectionHeader<SectionLocation>>

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
      ReadMetadataTables: Reader<ParsedMetadataTables, 'State>
      HandleError: ErrorHandler<'State> }

[<RequireQualifiedAccess>]
module MetadataReader =
    let skip (_: uint64): 'State -> _ = id

    let inline read reader arg =
        match reader with
        | ValueSome reader' -> reader' arg
        | ValueNone -> skip

    let readStreamHeader { ReadStreamHeader = reader } header i offset =
        match reader with
        | ValueSome reader' -> reader' header i offset
        | ValueNone -> id

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
          ReadMetadataTables = ValueNone
          HandleError = throwOnError }
