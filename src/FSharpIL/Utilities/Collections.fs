[<AutoOpen>]
module internal FSharpIL.Utilities.Collections

open System.Collections.Immutable

module ImmSet =
    let inline intersect items (set: ImmutableSortedSet<_>) = set.Intersect items

let inline (|Empty|_|) col =
    if (^T : (member IsEmpty: bool) col) then Some() else None

let inline (|Contains|_|) item col =
    if (^T : (member Contains: (^Item -> bool)) col) item then Some() else None
