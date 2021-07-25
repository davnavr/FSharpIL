namespace FSharpIL.Utilities

[<AutoOpen>]
module internal StringPatterns =
    let inline (|ToString|) (value: 'T) = value.ToString()
