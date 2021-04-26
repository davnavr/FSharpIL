namespace FSharpIL.Reading

open System.Collections.Immutable

open FSharpIL.PortableExecutable

// TODO: Allow reading functions to end reading early by making the return value a voption.
// TODO: Allow functions access to current offset by supplying a uint64 parameter.
type Reader<'Arg, 'State> = ('Arg -> uint64 -> 'State -> 'State) voption

type ErrorHandler<'State> = ReadState -> ReadError -> uint64 -> 'State -> 'State

type ParsedCoffHeader = CoffHeader<uint16, uint16>
type ParsedStandardFields = StandardFields<PEImageKind, uint32, uint32 voption>

// TODO: Rename this to something else.
[<NoComparison; NoEquality>]
type MetadataReader<'State> =
    { ReadLfanew: Reader<uint32, 'State>
      ReadCoffHeader: Reader<ParsedCoffHeader, 'State>
      ReadStandardFields: Reader<ParsedStandardFields, 'State>
      ReadNTSpecificFields: Reader<NTSpecificFields<uint64, uint32 * uint32, uint32, uint64, uint32>, 'State>
      ReadDataDirectories: Reader<ImmutableArray<struct(uint32 * uint32)>, 'State>
      ReadSectionHeaders: Reader<ImmutableArray<SectionHeader<SectionLocation>>, 'State>
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
          HandleError = throwOnError }
