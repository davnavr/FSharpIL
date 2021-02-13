namespace FSharpIL.Metadata

open System.Collections.Generic

[<Sealed>]
type MutableTable<'Value when 'Value :> IIndexValue> internal (owner: IndexOwner) =
    let set = HashSet<'Value>()

    member _.Count = set.Count

    member _.GetEnumerator() = set.GetEnumerator()

    member _.GetIndex(value: 'Value) =
        owner.CheckOwner value
        if set.Add value
        then SimpleIndex(owner, value) |> Some
        else None

    interface IReadOnlyCollection<'Value> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
