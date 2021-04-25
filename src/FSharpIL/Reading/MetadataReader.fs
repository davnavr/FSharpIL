﻿namespace FSharpIL.Reading

open System.Collections.Immutable

open FSharpIL.PortableExecutable

// TODO: Allow reading functions to end reading early by making the return value a voption.
type Reader<'Arg, 'State> = ('Arg -> 'State -> 'State) voption

// TODO: Rename this to something else.
[<NoComparison; NoEquality>]
type MetadataReader<'State> =
    { ReadLfanew: Reader<uint32, 'State>
      ReadCoffHeader: Reader<CoffHeader<uint16, uint16>, 'State>
      ReadStandardFields: Reader<StandardFields<PEImageKind, uint32, uint32 voption>, 'State>
      ReadNTSpecificFields: Reader<NTSpecificFields<uint64, uint32 * uint32, uint32, uint64, uint32>, 'State>
      ReadDataDirectories: Reader<ImmutableArray<struct(uint32 * uint32)>, 'State>
      HandleError: uint64 -> ReadState -> ReadError -> 'State -> 'State }

[<RequireQualifiedAccess>]
module MetadataReader =
    let inline private read reader arg =
        match reader with
        | ValueSome reader' -> reader' arg
        | ValueNone -> id

    let readLfanew { ReadLfanew = reader } lfanew = read reader lfanew
    let readCoffHeader { ReadCoffHeader = reader } header = read reader header
    let readStandardFields { ReadStandardFields = reader } fields = read reader fields
    let readNTSpecificFields { ReadNTSpecificFields = reader } fields = read reader fields
    let readDataDirectories { ReadDataDirectories = reader } directories = read reader directories

    let inline throwOnError (offset: uint64) (state: ReadState) error (_: 'State): 'State =
        ReadException(offset, state, error) |> raise

    [<GeneralizableValue>]
    let empty: MetadataReader<'State> =
        { ReadLfanew = ValueNone
          ReadCoffHeader = ValueNone
          ReadStandardFields = ValueNone
          ReadNTSpecificFields = ValueNone
          ReadDataDirectories = ValueNone
          HandleError = throwOnError }
