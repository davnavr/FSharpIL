namespace FSharpIL.Metadata

open System
open System.Collections.Generic

#nowarn "342" // Class that implements IComparable should override Object.Equals

type IndexOwner internal () = class end

type IIndexValue =
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException">The owner objects do not refer to the same object.</exception>
    abstract CheckOwner: IndexOwner -> unit

type IIndex = abstract Owner: IndexOwner

// TODO: See if this being a struct is a performance advantage.
[<CustomComparison; StructuralEquality>]
[<System.Runtime.CompilerServices.IsReadOnlyAttribute; Struct>]
type SimpleIndex<'Value when 'Value : equality> internal (owner: IndexOwner, value: 'Value) =
    member internal _.Owner = owner
    member _.Value = value
    override _.ToString() = value.ToString()

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
    override _.ToString() = index.ToString()

    internal new(owner, value) = TaggedIndex(SimpleIndex<_>(owner, value))

    interface IIndex with member this.Owner = this.Owner

/// The exception that is thrown if the owner of two objects do not refer to the same object.
exception IndexOwnerMismatchException of expected: IndexOwner * actual: IndexOwner

[<RequireQualifiedAccess>]
module internal IndexOwner =
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException">
    /// The owner objects <paramref name="one"/> and <paramref name="two"/> do not refer to the same object.
    /// </exception>
    let ensureEqual (one: IndexOwner) (two: IndexOwner) =
        if Object.ReferenceEquals(one, two) |> not then
            IndexOwnerMismatchException(one, two) |> raise

    /// <summary>Checks that the specified object is owned by the correct <see cref="T:FSharpIL.Metadata.IndexOwner"/>.</summary>
    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException">
    /// The owner of the <paramref name="value"/> and the expected <paramref name="owner"/> do not refer to the same object.
    /// </exception>
    let inline checkOwner owner (value: #IIndexValue) = value.CheckOwner owner

    /// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
    let checkIndex owner (index: SimpleIndex<_>) =
        ensureEqual owner index.Owner
        checkOwner owner index.Value // TODO: Fix, this function call might be redundant.

[<AutoOpen>]
module internal IndexHelpers =
    let inline (|SimpleIndex|) (index: ^Index) =
        (^Index : (member Index : SimpleIndex<'T>) index)

    let (|IndexOwner|) (index: #IIndex) = index.Owner
    let (|IndexValue|) (value: #IIndexValue) = value :> IIndexValue
