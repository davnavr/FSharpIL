namespace FSharpIL.Metadata

open System
open System.Collections.Generic

type IndexOwner internal () =
    member internal this.EnsureEqual(other: IndexOwner) =
        if Object.ReferenceEquals(this, other) |> not then
            invalidOp "Cannot use an object owned by another state"

    member internal this.CheckOwner(value: #IIndexValue) = value.CheckOwner this

and IIndexValue =
    abstract CheckOwner: IndexOwner -> unit

type IIndex = abstract Owner: IndexOwner

// TODO: Figure out how to allow equality and comparison? This problem might be made easier if multiple specific handle types existed.
// TODO: See if this being a struct is a performance advantage.
// TODO: Consider creating unique index types for each table
[<CustomEquality; CustomComparison>]
[<System.Runtime.CompilerServices.IsReadOnlyAttribute; Struct>]
type SimpleIndex<'Value> internal (owner: IndexOwner, value: 'Value) =
    member _.Owner = owner
    member _.Value = value

    interface IIndex with member this.Owner = this.Owner

    static member inline private Equals(one: SimpleIndex<'Value>, two: SimpleIndex<'Value>) =
        one.Owner = two.Owner && EqualityComparer.Default.Equals(one.Value, two.Value)

    override this.Equals obj = SimpleIndex<_>.Equals(this, obj :?> SimpleIndex<_>)

    interface IEquatable<SimpleIndex<'Value>> with
        member this.Equals other = SimpleIndex<_>.Equals(this, other)

    static member inline private CompareTo(one, two) = Comparer<SimpleIndex<'Value>>.Default.Compare(one, two)

    interface IComparable<SimpleIndex<'Value>> with
        member this.CompareTo other = SimpleIndex<_>.CompareTo(this, other)

    interface IComparable with
        member this.CompareTo obj = SimpleIndex<_>.CompareTo(this, obj :?> SimpleIndex<_>)

    override this.GetHashCode() = 1 // TODO: Figure out how to make this work.

[<StructuralComparison; StructuralEquality>]
[<System.Runtime.CompilerServices.IsReadOnlyAttribute; Struct>]
type TaggedIndex<'Tag, 'Value> internal (index: SimpleIndex<'Value>) =
    member _.Index = index
    member _.Owner = index.Owner
    member _.Value = index.Value

    internal new(owner, value) = TaggedIndex(SimpleIndex<_>(owner, value))

    interface IIndex with member this.Owner = this.Owner

[<AutoOpen>]
module IndexHelpers =
    let inline (|SimpleIndex|) (index: ^Index) =
        (^Index : (member Index : SimpleIndex<'T>) index)

    let internal (|IndexOwner|) (index: #IIndex) = index.Owner
