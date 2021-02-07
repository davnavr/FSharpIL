namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

/// Represents a list of methods or fields.
[<Struct; System.Runtime.CompilerServices.IsReadOnly>]
type MemberList<'Member, 'Row> internal (members: ImmutableArray<'Row>) =
    member _.Count = members.Length
    member _.Item with get i = members.Item i
    member _.Contains mber = members.Contains mber
    member _.GetEnumerator() = members.GetEnumerator
    member _.IndexOf mber = members.IndexOf mber
    member internal _.ToImmutableArray() = members

    static member Empty = MemberList<'Member, 'Row> ImmutableArray.Empty

    interface IReadOnlyList<'Row> with
        member this.Count = this.Count
        member this.Item with get i = this.Item i
        member _.GetEnumerator() = (members :> IEnumerable<_>).GetEnumerator()
        member _.GetEnumerator() = (members :> System.Collections.IEnumerable).GetEnumerator()

[<Sealed>]
type MemberListBuilder<'Member, 'Row> internal (row: 'Member -> 'Row) =
    member inline _.Combine(x: _ -> Result<_, 'Member>, y: _ -> Result<unit, _>) =
        fun (set: HashSet<'Member>) -> x set |> Result.bind (fun () -> y set)
    member inline _.Delay(f: unit -> HashSet<'Row> -> Result<unit, 'Member>) = fun set -> f () set
    member _.Run (expr: _ -> Result<_, 'Member>) =
        let set = HashSet<'Row>()
        match expr set with
        | Ok() -> set.ToImmutableArray() |> MemberList<'Member, 'Row> |> Ok
        | Error err -> Error err
    member _.Yield mber =
        fun (set: HashSet<'Row>) ->
            let item = row mber
            if set.Add item
            then Ok()
            else Error mber
    member inline _.Zero() = fun (_: HashSet<'Row>) -> Result<_, 'Member>.Ok()
