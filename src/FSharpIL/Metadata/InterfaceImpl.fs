namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

type InterfaceIndexTag =
    | TypeDef = 0uy
    | TypeRef = 1uy
    | TypeSpec = 2uy

// TODO: Replace InterfaceIndex with TypeDefOrRefOrSpec as used in Items.fs
/// <summary>Specifies an interface that is implemented by a <c>TypeDef</c> (II.22.23).</summary>
type InterfaceIndex = TaggedIndex<InterfaceIndexTag>

[<RequireQualifiedAccess>]
module InterfaceIndex =
    let (|TypeDef|TypeRef|TypeSpec|) (index: InterfaceIndex) =
        match index.Tag with
        | InterfaceIndexTag.TypeRef -> TypeRef(index.ToRawIndex<TypeRef>())
        | InterfaceIndexTag.TypeSpec -> TypeSpec(index.ToRawIndex<TypeSpecRow>())
        | InterfaceIndexTag.TypeDef
        | _ -> TypeDef(index.ToRawIndex<InterfaceDef>())

    let TypeDef (index: RawIndex<InterfaceDef>) = index.ToTaggedIndex InterfaceIndexTag.TypeDef
    let TypeRef (index: RawIndex<TypeRef>) = index.ToTaggedIndex InterfaceIndexTag.TypeRef
    let TypeSpec (index: RawIndex<TypeSpecRow>) = index.ToTaggedIndex InterfaceIndexTag.TypeSpec

[<Sealed>]
type internal InterfaceIndexEntries internal () =
    let items = List<InterfaceIndex>()
    let lookup = HashSet<InterfaceIndex>()

    member _.Count = items.Count

    member _.Add(intf, duplicate: outref<bool>) =
        duplicate <- lookup.Add intf |> not
        items.Add intf

    member _.GetEnumerator() = items.GetEnumerator()

/// <summary>
/// (0x09) Represents a row in the <c>InterfaceImpl</c> table, which specifies the interfaces implemented by types (II.22.23).
/// </summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type InterfaceImpl =
    { Class: RawIndex<TypeDefRow>
      Interface: InterfaceIndex }

type internal InterfaceImplLookup = IReadOnlyDictionary<RawIndex<TypeDefRow>, ImmutableArray<RawIndex<InterfaceImpl>>>

[<Sealed>]
type InterfaceImplTable internal (rows: ImmutableArray<InterfaceImpl>, lookup: InterfaceImplLookup) =
    member _.Count = rows.Length
    member _.Rows = rows
    member _.Item with get (index: RawIndex<InterfaceImpl>) = &rows.ItemRef(index.Value - 1)

    member _.GetIndices (tindex: RawIndex<TypeDefRow>) = lookup.[tindex]

    interface IMetadataTable<InterfaceImpl> with
        member this.Count = this.Count
        member this.Item with get index = &this.[index]

[<Sealed>]
type InterfaceImplTableBuilder internal () =
    let mapping = Dictionary<RawIndex<TypeDefRow>, InterfaceIndexEntries>()
    let mutable count = 0

    member _.Count = count

    /// <param name="typeRow">The <c>TypeDef</c> that will implement the interface.</param>
    /// <param name="intf">The interface to implement.</param>
    /// <returns>
    /// <see langword="true"/>, if the entry was not a duplicate and the row was successfully added; otherwise
    /// <see langword="false"/>.
    /// </returns>
    member _.Add(typeRow, intf) =
        let impls =
            match mapping.TryGetValue typeRow with
            | (true, existing) -> existing
            | (false, _) ->
                let empty = InterfaceIndexEntries()
                mapping.[typeRow] <- empty
                empty
        count <- count + 1
        let mutable duplicate = false
        impls.Add(intf, &duplicate)
        not duplicate

    member internal _.ToImmutable() =
        let rows = ImmutableArray.CreateBuilder count
        let lookup = Dictionary count
        for KeyValue(typeDef, entries) in mapping do
            let iset = HashSet entries.Count
            for impl in entries do
                rows.Add { Class = typeDef; Interface = impl }
                if not (iset.Add(RawIndex rows.Count)) then failwith "Duplicate interface index detected"
            lookup.[typeDef] <- iset.ToImmutableArray()
        InterfaceImplTable(rows.ToImmutable(), lookup)
