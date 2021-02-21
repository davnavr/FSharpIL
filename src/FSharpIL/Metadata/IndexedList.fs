namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

[<Sealed>]
type internal IndexedList<'T when 'T : equality and 'T :> IIndexValue> (owner: IndexOwner) =
    let lookup = HashSet<'T>()
    let items = ImmutableArray.CreateBuilder<'T>()

    member _.Count = items.Count
    member _.ToImmutable() = items.ToImmutable()

    member _.Add(value: 'T) =
        IndexOwner.checkOwner owner value
        if lookup.Add value
        then
            let i = items.Count
            items.Add value
            ValueSome i
        else ValueNone
