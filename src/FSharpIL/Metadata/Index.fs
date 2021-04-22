namespace rec FSharpIL.Metadata

open System.Runtime.CompilerServices

// TODO: Rename to RowIndex or Index
/// Represents an index into a metadata table.
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type RawIndex<'Tag> internal (value: int32) =
    internal new (value: uint32) = RawIndex(int32 value)
    /// <remarks>A value of <c>1</c> refers to the first row in the metadata table.</remarks>
    member _.Value = value
    member internal _.ToTaggedIndex(tag) = TaggedIndex(tag, value)
    member internal _.ChangeTag<'To>() =
        let mutable value' = value
        Unsafe.As<_, RawIndex<'To>> &value'
    override _.ToString() = sprintf "%s(0x%x)" typeof<'Tag>.Name value
    static member op_Implicit(index: RawIndex<'Tag>) = index.Value
    static member op_Implicit(index: RawIndex<'Tag>) = uint32 index.Value
    static member op_Implicit(index: RawIndex<'Tag>) = uint16 index.Value

/// Represents an index into one or more metadata tables.
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type TaggedIndex<'Tag when 'Tag : struct> internal (tag: 'Tag, value: int32) =
    internal new (tag: 'Tag) = TaggedIndex(tag, 0)
    member _.Tag = tag
    member _.Value = value
    member internal _.ToRawIndex<'To>() = RawIndex<'To> value
    override _.ToString() = sprintf "%O(0x%x)" tag value

// TODO: Create separate index type for OwnedMetadataTable and parameters.

[<AutoOpen>]
module internal IndexHelpers =
    let (|Index|) (index: RawIndex<_>) = index.Value
