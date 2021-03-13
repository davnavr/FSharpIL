namespace FSharpIL.Metadata

open System.Collections.Generic

[<Sealed>]
type MetadataTableBuilder<'T when 'T :> IIndexValue and 'T : equality> internal (owner: IndexOwner) =
    let items = RowHashSet<'T> owner

    member _.Count = items.Count
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    member _.TryAdd(item: 'T) =
        item.CheckOwner owner
        items.TryAdd item
    member _.GetEnumerator() = items.GetEnumerator()

    interface IReadOnlyCollection<'T> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator()
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
