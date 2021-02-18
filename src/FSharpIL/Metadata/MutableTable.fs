namespace FSharpIL.Metadata

open System.Collections.Generic

// TODO: Rename to MetadataTableBuilder?
[<Sealed>]
type MutableTable<'Value when 'Value :> IIndexValue and 'Value : equality> internal (owner: IndexOwner) =
    let set = HashSet<'Value>()

    member _.Count = set.Count

    member _.GetEnumerator() = set.GetEnumerator()

    member _.GetIndex(value: 'Value) = // TODO: Use value option.
        IndexOwner.checkOwner owner value
        if set.Add value
        then SimpleIndex(owner, value) |> Some
        else None

    interface IReadOnlyCollection<'Value> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
