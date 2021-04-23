namespace FSharpIL.Reading

type MetadataReader<'State> =
    { ReadLfanew: (uint32 -> 'State -> 'State) voption
      HandleError: 'State -> ReadState -> ReadError -> 'State }

[<RequireQualifiedAccess>]
module MetadataReader =
    let throwOnError (_: 'State) (_: ReadState) error: 'State = raise(ReadException error)
