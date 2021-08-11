namespace FSharpIL.Reading

open FSharpIL.PortableExecutable

type ErrorHandler<'State> = IReadState -> ReadError -> FileOffset -> 'State -> 'State

[<RequireQualifiedAccess>]
module ErrorHandler =
    let inline handle (rstate: #IReadState) error offset ustate (handler: ErrorHandler<_>) = handler rstate error offset ustate
    let inline throwOnError (state: IReadState) error offset (_: 'State): 'State = ReadException(state, error, offset) |> raise
