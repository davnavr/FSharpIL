namespace FSharpIL.Metadata

type IHandle =
    abstract Owner : obj
    abstract ValueType : System.Type

type IHandleValue =
    abstract Handles: seq<IHandle>

/// <summary>
/// Guarantees that values originate from the same <see cref="FSharpIL.Metadata.MetadataBuilderState"/>.
/// </summary>
type Handle<'Value> =
    private
    | Handle of obj * 'Value

    member this.Item = let (Handle (_, value)) = this in value

    interface IHandle with
        member this.Owner = let (Handle (owner, _)) = this in owner
        member this.ValueType = this.Item.GetType()

[<AutoOpen>]
module internal HandleHelpers =
    let inline (|Handle|) (handle: #IHandle) = handle :> IHandle
