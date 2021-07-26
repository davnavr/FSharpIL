[<RequireQualifiedAccess>]
module FSharpIL.Utilities.Flags

/// Determines whether the specified flags are set.
let inline set (flags: 'Enum) (value: 'Enum) = value &&& flags = flags

let inline anyOfSet (flags: 'Enum) (value: 'Enum) = value &&& flags > Unchecked.defaultof<'Enum>
