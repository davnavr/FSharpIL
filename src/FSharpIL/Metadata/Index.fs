namespace FSharpIL.Metadata

open System
open System.Collections.Generic

#nowarn "342" // Class that implements IComparable should override Object.Equals

type IndexOwner internal () =
    member internal this.EnsureEqual(other: IndexOwner) =
        if Object.ReferenceEquals(this, other) |> not then
            invalidOp "Cannot use an object owned by another state"

    /// <exception cref="T:System.InvalidOperation"/>
    member internal this.CheckOwner(value: #IIndexValue) = value.CheckOwner this

and IIndexValue =
    /// <exception cref="T:System.InvalidOperation"/>
    abstract CheckOwner: IndexOwner -> unit

type IIndex = abstract Owner: IndexOwner

// TODO: See if this being a struct is a performance advantage.
[<CustomComparison; StructuralEquality>]
[<System.Runtime.CompilerServices.IsReadOnlyAttribute; Struct>]
type SimpleIndex<'Value when 'Value : equality> internal (owner: IndexOwner, value: 'Value) =
    member internal _.Owner = owner
    member _.Value = value

    interface IIndex with member this.Owner = this.Owner

    interface IComparable with
        member _.CompareTo obj =
            let other = obj :?> SimpleIndex<_>
            Comparer<'Value>.Default.Compare(value, other.Value)

[<StructuralComparison; StructuralEquality>]
[<System.Runtime.CompilerServices.IsReadOnlyAttribute; Struct>]
type TaggedIndex<'Tag, 'Value when 'Value : equality> internal (index: SimpleIndex<'Value>) =
    member _.Index = index
    member internal _.Owner = index.Owner
    member _.Value = index.Value

    internal new(owner, value) = TaggedIndex(SimpleIndex<_>(owner, value))

    interface IIndex with member this.Owner = this.Owner

[<AutoOpen>]
module IndexHelpers =
    let inline (|SimpleIndex|) (index: ^Index) =
        (^Index : (member Index : SimpleIndex<'T>) index)

    let internal (|IndexOwner|) (index: #IIndex) = index.Owner
