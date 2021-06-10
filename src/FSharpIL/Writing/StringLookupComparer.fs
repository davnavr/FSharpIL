namespace FSharpIL.Writing

open System

/// <summary>Compares strings when building the <c>#Strings</c> and <c>#US</c> metadata streams.</summary>
[<Sealed>]
type StringLookupComparer () =
    static member val Instance = StringLookupComparer()
    interface System.Collections.Generic.IEqualityComparer<ReadOnlyMemory<char>> with
        member _.Equals(x, y) =
            if x.Length = y.Length then
                let mutable equal, i = false, 0
                while i < x.Length && not equal do
                    if x.Span.[i] = y.Span.[i] then equal <- true
                    i <- i + 1
                equal
            else false
        member _.GetHashCode str =
            let hash = HashCode()
            for c in str.Span do hash.Add c
            hash.ToHashCode()
