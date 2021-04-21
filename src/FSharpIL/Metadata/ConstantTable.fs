namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ConstantParent =
    let (|Field|Param|Property|) (parent: ConstantParent) =
        match parent.Tag with
        | ConstantParentTag.Field -> Field(parent.ToRawIndex<FieldRow>())
        | ConstantParentTag.Param -> Param(parent.ToRawIndex<ParamRow>())
        | ConstantParentTag.Property -> Property(parent.ToRawIndex<PropertyRow>())
        | _ -> invalidArg "parent" "Invalid constant parent tag"

    let Field (index: RawIndex<FieldRow>) = index.ToTaggedIndex ConstantParentTag.Field
    let Param (index: RawIndex<ParamRow>) = index.ToTaggedIndex ConstantParentTag.Param
    let Property (index: RawIndex<PropertyRow>) = index.ToTaggedIndex ConstantParentTag.Property

[<Sealed>]
type ConstantTable internal
    (
        lookup: IImmutableDictionary<ConstantParent, RawIndex<ConstantRow>>,
        rows: ImmutableArray<ConstantRow>
    ) =

    member _.Count = rows.Length
    member _.Rows = rows
    member _.Item with get (index: RawIndex<ConstantRow>) = &rows.ItemRef(index.Value - 1)

    member _.TryGetIndex parent =
        match lookup.TryGetValue parent with
        | true, index -> ValueSome index
        | false, _ -> ValueNone

    member this.TryGetRow parent = this.TryGetIndex parent |> ValueOption.map (fun i -> this.[i])

    interface IMetadataTable<ConstantRow> with
        member this.Count = this.Count
        member this.Item with get index = &this.[index]

[<Sealed>]
type ConstantTableBuilder internal () =
    let lookup = ImmutableSortedDictionary.CreateBuilder<ConstantParent, RawIndex<ConstantRow>>()
    let rows = ImmutableArray.CreateBuilder<ConstantRow>()

    member _.Count = rows.Count

    member _.TryAdd(parent, value) =
        let i = RawIndex(rows.Count + 1)
        if lookup.TryAdd(parent, i) then
            rows.Add(ConstantRow(parent, value))
            ValueSome i
        else ValueNone

    member this.TryAdd(parent: RawIndex<FieldRow>, value) = this.TryAdd(ConstantParent.Field parent, value)
    member this.TryAdd(parent: RawIndex<ParamRow>, value) = this.TryAdd(ConstantParent.Param parent, value)
    member this.TryAdd(parent: RawIndex<PropertyRow>, value) = this.TryAdd(ConstantParent.Property parent, value)

    // TODO: Sort constant rows by parent.
    member internal _.ToImmutable() = ConstantTable(lookup.ToImmutable(), rows.ToImmutable())
