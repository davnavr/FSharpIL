namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

/// Contains the rows of a metadata table, and allows the addition of duplicate rows.
[<System.Runtime.CompilerServices.IsReadOnly>]
type RowArrayList<'Row when 'Row : equality> private (items: ImmutableArray<'Row>.Builder, lookup: HashSet<'Row>) = struct
    member _.Count = items.Count
    member _.Item with get (index: RawIndex<'Row>) = items.[index.Value - 1]

    /// <param name="item">The element to add to this list.</param>
    /// <param name="duplicate">
    /// When this method returns, contains <see langword="true"/> if the item added was a duplicate; otherwise, contains
    /// <see langword="false"/>.
    /// </param>
    member _.Add(item, duplicate: outref<bool>) =
        items.Add item
        duplicate <- lookup.Add item |> not
        RawIndex<'Row> items.Count

    member _.GetEnumerator() = items.GetEnumerator()

    member internal _.ToImmutable() = MetadataTable<'Row>(items.ToImmutable())

    static member Create() = RowArrayList<'Row>(ImmutableArray.CreateBuilder<'Row>(), HashSet<_>())

    interface IReadOnlyCollection<'Row> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator()
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator

    interface IMetadataTable<'Row> with
        member this.Count = this.Count
        member this.Item with get index = this.[index]
end
