namespace FSharpIL

/// Indicates that the value of a field cannot be set.
type [<System.Runtime.CompilerServices.IsReadOnly>] Omitted = struct end

[<AutoOpen>]
module Omitted = let Omitted = Omitted()
