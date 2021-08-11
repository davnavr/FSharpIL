namespace ILInfo

open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type IncludeHeaders =
    | NoHeaders
    | IncludeHeaders

[<IsReadOnly; Struct>]
type IncludeMetadata =
    | IncludeMetadata
    | NoMetadata

[<RequireQualifiedAccess>]
type OutputKind =
    | Console
    | File of string
