namespace ILInfo

open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type IncludeHeaders =
    | NoHeaders
    | IncludeHeaders

[<IsReadOnly; Struct>]
type IncludeIL =
    | IncludeIL
    | NoIL

[<RequireQualifiedAccess>]
type OutputKind =
    | Console
    | File of string
