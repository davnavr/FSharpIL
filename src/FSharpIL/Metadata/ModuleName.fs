namespace FSharpIL.Metadata

/// NOTE: Must be non-empty string.
type ModuleName =
    internal
    | ModuleName of string

[<RequireQualifiedAccess>]
module ModuleName =
    let thing = ()
