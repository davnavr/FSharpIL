namespace FSharpIL.Reading

open FSharpIL.PortableExecutable

type Reader<'Arg, 'State> = ('Arg -> 'State -> 'State) voption

// TODO: Rename this to something else.
[<NoComparison; NoEquality>]
type MetadataReader<'State> =
    { ReadLfanew: Reader<uint32, 'State>
      ReadCoffHeader: Reader<CoffHeader<uint16, uint16>, 'State>
      ReadStandardFields: Reader<StandardFields<PEImageKind, uint32, uint32 voption>, 'State>
      ReadNTSpecificFields: Reader<NTSpecificFields<uint64, uint32 * uint32, uint32, uint64, uint32>, 'State>
      HandleError: uint64 -> ReadState -> ReadError -> 'State -> 'State }

[<RequireQualifiedAccess>]
module MetadataReader =
    let inline private read reader arg state =
        match reader with
        | ValueSome reader' -> reader' arg state
        | ValueNone -> state

    let readLfanew { ReadLfanew = reader } lfanew = read reader lfanew
    let readCoffHeader { ReadCoffHeader = reader } header = read reader header
    let readStandardFields { ReadStandardFields = reader } fields = read reader fields
    let readNTSpecificFields { ReadNTSpecificFields = reader } fields = read reader fields

    let inline throwOnError (offset: uint64) (state: ReadState) error (_: 'State): 'State =
        ReadException(offset, state, error) |> raise

    [<GeneralizableValue>]
    let empty: MetadataReader<'State> =
        { ReadLfanew = ValueNone
          ReadCoffHeader = ValueNone
          ReadStandardFields = ValueNone
          ReadNTSpecificFields = ValueNone
          HandleError = throwOnError }
