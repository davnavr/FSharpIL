[<AutoOpen>]
module internal FSharpIL.Utilities.Collections

open System.Collections.Generic
open System.Collections.Immutable

module ImmSet =
    let empty<'T> = ImmutableSortedSet<'T>.Empty
    let withComparer comparer (set: ImmutableSortedSet<_>) =
        { new IComparer<_> with
            member _.Compare(e1, e2) = comparer e1 e2 }
        |> set.WithComparer

    let inline intersect items (set: ImmutableSortedSet<_>) = set.Intersect items

let inline (|Empty|_|) col =
    if (^T : (member IsEmpty: bool) col) then Some() else None

let inline (|Contains|_|) item col =
    if (^T : (member Contains: (^Item -> bool)) col) item then Some() else None
