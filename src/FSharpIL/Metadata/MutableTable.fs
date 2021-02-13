namespace FSharpIL.Metadata

open System.Collections.Generic

[<Sealed>]
type MutableTable<'Value> internal (owner: obj) =
    let set = HashSet<'Value>()

    member _.Count = set.Count

    member _.GetEnumerator() = set.GetEnumerator()

    member _.GetIndex(value: 'Value) =
        // state.EnsureOwner value // TODO: Check that "sub" indices match the owner.
        if set.Add value
        then SimpleIndex(owner, value) |> Some
        else None

    interface IReadOnlyCollection<'Value> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
