namespace FSharpIL.Metadata

open System.Collections.Generic

type IHandle =
    abstract Owner : obj
    abstract ValueType : System.Type

type IHandleValue =
    abstract Handles: seq<IHandle>

/// <summary>
/// Guarantees that values originate from the same <see cref="T:FSharpIL.Metadata.MetadataBuilderState"/>.
/// </summary>
[<Struct; System.Runtime.CompilerServices.IsReadOnly>]
// TODO: See if this being a struct is a performance advantage.
// TODO: Should this be a record instead of a union to avoid the generation of a "Tag" member?
type Handle<'Value> =
    private
    | Handle of obj * 'Value

    member this.Owner = let (Handle (owner, _)) = this in owner
    member this.Item = let (Handle (_, value)) = this in value
    member this.ValueType = this.Item.GetType()

    interface IHandle with
        member this.Owner = this.Owner
        member this.ValueType = this.ValueType

[<Sealed>]
type internal HandleEqualityComparer<'Value>(valueComparer: IEqualityComparer<'Value>) =
    interface IEqualityComparer<Handle<'Value>> with
        member _.Equals(Handle (x, xval), Handle (y, yval)) =
            x = y && valueComparer.Equals(xval, yval)
        member _.GetHashCode value = valueComparer.GetHashCode value.Item

[<AutoOpen>]
module internal HandlePatterns =
    let inline (|IHandle|) (handle: #IHandle) = handle :> IHandle
    let inline (|Handles|) (value: #IHandleValue) = value.Handles
    let inline (|HandleValue|) (handle: Handle<'T>) = handle.Item
