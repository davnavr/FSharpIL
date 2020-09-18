[<AutoOpen>]
module internal FSharpIL.Utilities.Collections

open System.Collections.Generic
open System.Collections.Immutable

[<RequireQualifiedAccess>]
module Comparer =
    let create comparer =
        { new IComparer<_> with
              member _.Compare(e1, e2) = comparer e1 e2}

[<RequireQualifiedAccess>]
module SortedSet =
    let inline intersect items (set: ImmutableSortedSet<_>) = set.Intersect items

let inline (|Empty|_|) col =
    if (^T : (member IsEmpty: bool) col) then Some() else None

let inline (|Contains|_|) item col =
    if (^T : (member Contains: (^Item -> bool)) col) item then Some() else None
