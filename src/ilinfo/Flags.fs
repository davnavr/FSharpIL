namespace ILInfo

open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type IncludeHeaders =
    | NoHeaders
    | IncludeHeaders

[<RequireQualifiedAccess>]
type OutputKind =
    | Console
    | File of string
