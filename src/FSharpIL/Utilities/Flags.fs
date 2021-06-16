[<RequireQualifiedAccess>]
module FSharpIL.Utilities.Flags

/// Determines whether the specified flags are set.
let inline set (flags: 'Enum) (value: 'Enum) = value &&& flags <> Unchecked.defaultof<'Enum>

let inline anyOfSet (flags: 'Enum) (value: 'Enum) = value &&& flags > Unchecked.defaultof<'Enum>
