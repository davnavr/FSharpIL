namespace FSharpIL.Reading

open FSharpIL.PortableExecutable

type ErrorHandler<'State> = ReadState -> ReadError -> FileOffset -> 'State -> 'State

[<RequireQualifiedAccess>]
module ErrorHandler =
    let inline throwOnError (state: ReadState) error offset (_: 'State): 'State =
        ReadException(state, error, offset) |> raise
