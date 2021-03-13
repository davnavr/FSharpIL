namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

/// Contains the rows of a metadata table, and allows the addition of duplicate rows.
[<System.Runtime.CompilerServices.IsReadOnly>]
type RowArrayList<'T when 'T : equality> = struct
    val private items: ImmutableArray<'T>.Builder
    val private lookup: HashSet<'T>
    val private owner: IndexOwner

    internal new (owner) =
        { items = ImmutableArray.CreateBuilder<_>()
          lookup = HashSet<_>()
          owner = owner }

    member this.Count = this.items.Count
    /// <param name="item">The element to add to this list.</param>
    /// <param name="duplicate">
    /// When this method returns, contains <see langword="true"/> if the item added was a duplicate; otherwise, contains
    /// <see langword="false"/>.
    /// </param>
    member this.Add(item, duplicate: outref<bool>) =
        this.items.Add item
        duplicate <- this.lookup.Add item |> not
        SimpleIndex<'T>(this.owner, item)
    member this.GetEnumerator() = this.items.GetEnumerator()
    member this.ToImmutableArray() = this.items.ToImmutable()

    interface IReadOnlyCollection<'T> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator()
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
end
