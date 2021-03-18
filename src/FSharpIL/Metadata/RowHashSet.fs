namespace FSharpIL.Metadata

open System.Collections.Generic

/// Contains the rows of a metadata table, and prevents the addition of duplicate rows.
[<System.Runtime.CompilerServices.IsReadOnly>]
type RowHashSet<'Row when 'Row : equality> private (items: RowArrayList<'Row>) = struct
    member _.Count = items.Count
    member _.Item with get index = items.[index]

    member _.TryAdd item =
        let mutable duplicate = false
        let i = items.Add(item, &duplicate)
        if duplicate then ValueNone else ValueSome i

    member _.GetEnumerator() = items.GetEnumerator()

    member internal _.ToImmutable() = items.ToImmutable()

    static member internal Create() = RowHashSet<'Row>(RowArrayList<_>.Create())

    interface IReadOnlyCollection<'Row> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator

    interface IMetadataTable<'Row> with
        member this.Count = this.Count
        member this.Item with get index = this.[index]
end
