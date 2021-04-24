namespace FSharpIL.Reading

open FSharpIL.PortableExecutable

type ReadStandardFields<'State> =
    OptionalHeaderMagic -> StandardFields -> uint32 -> uint32 -> uint32 -> uint32 -> uint32 -> uint32 voption -> 'State -> 'State

// TODO: Consider making this a class.
// TODO: Rename this to something else.
type MetadataReader<'State> =
    { ReadLfanew: (uint32 -> 'State -> 'State) voption
      ReadCoffHeader: (CoffHeader -> uint16 -> uint16 -> 'State -> 'State) voption
      ReadStandardFields: ReadStandardFields<'State> voption
      HandleError: uint64 -> ReadState -> ReadError -> 'State -> 'State }

[<RequireQualifiedAccess>]
module MetadataReader =
    let inline private read reader call =
        match reader with
        | ValueSome reader' -> call reader'
        | ValueNone -> id

    let readLfanew { ReadLfanew = reader } lfanew = read reader (fun reader' -> reader' lfanew)

    let readCoffHeader { ReadCoffHeader = reader } header numberOfSections optionalHeaderSize =
        read reader (fun reader' -> reader' header numberOfSections optionalHeaderSize)

    let readStandardFields
        { ReadStandardFields = reader }
        magic
        lmajor
        lminor
        codeSize
        initDataSize
        uninitDataSize
        entryPointRva
        baseOfCode
        baseOfData
        =
        fun reader' ->
            reader'
                magic
                { LMajor = lmajor; LMinor = lminor }
                codeSize
                initDataSize
                uninitDataSize
                entryPointRva
                baseOfCode
                baseOfData
        |> read reader

    let inline throwOnError (offset: uint64) (state: ReadState) error (_: 'State): 'State =
        ReadException(offset, state, error) |> raise

    [<GeneralizableValue>]
    let empty: MetadataReader<'State> =
        { ReadLfanew = ValueNone
          ReadCoffHeader = ValueNone
          ReadStandardFields = ValueNone
          HandleError = throwOnError }
