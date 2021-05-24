namespace FSharpIL.PortableExecutable

/// Indicates that the value of a field cannot be set.
type Omitted = struct end

[<AutoOpen>]
module Omitted = let Omitted = Omitted()
