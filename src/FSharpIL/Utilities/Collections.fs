[<AutoOpen; System.ObsoleteAttribute>]
module internal FSharpIL.Utilities.Collections

open System.Collections.Generic

[<RequireQualifiedAccess>]
module Comparer =
    let create comparer =
        { new IComparer<_> with
              member _.Compare(e1, e2) = comparer e1 e2 }

let inline (|Empty|_|) col =
    if (^T : (member IsEmpty: bool) col) then Some() else None

let inline (|Contains|_|) item col =
    if (^T : (member Contains: (^Item -> bool)) col) item then Some() else None
