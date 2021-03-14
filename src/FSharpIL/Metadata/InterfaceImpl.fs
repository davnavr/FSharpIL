namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

/// <summary>Specifies an interface that is implemented by a <c>TypeDef</c> (II.22.23).</summary>
[<RequireQualifiedAccess>]
[<NoComparison; StructuralEquality>]
type InterfaceIndex =
    | TypeDef of TypeDefIndex<InterfaceDef>
    | TypeRef of SimpleIndex<TypeRef>
    | TypeSpec of SimpleIndex<TypeSpecRow>

    override this.ToString() =
        match this with
        | TypeDef(SimpleIndex tdef) -> tdef.Value.GetFullName()
        | TypeRef tref -> tref.Value.ToString()
        | TypeSpec tspec -> tspec.Value.ToString()

    interface IIndexValue with
        member this.CheckOwner owner =
            match this with
            | TypeDef(SimpleIndex tdef) -> IndexOwner.checkIndex owner tdef
            | TypeRef tref -> IndexOwner.checkIndex owner tref
            | TypeSpec tspec -> IndexOwner.checkIndex owner tspec

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
    { Class: SimpleIndex<TypeDefRow>
      Interface: InterfaceIndex }

    override this.ToString() = sprintf "%O implements %O" (this.Class.Value.GetFullName()) this.Interface

type internal InterfaceImplLookup = IReadOnlyDictionary<SimpleIndex<TypeDefRow>, IReadOnlyDictionary<InterfaceIndex, int32>>

[<Sealed>]
type InterfaceImplTable internal (rows: ImmutableArray<InterfaceImpl>, lookup: InterfaceImplLookup) =
    member _.Count = rows.Length
    member _.Rows = rows
    member _.Item with get(tdef: SimpleIndex<TypeDefRow>, index: InterfaceIndex) = lookup.[tdef].[index]

    interface IMetadataTable<InterfaceImpl> with
        member this.Count = this.Count
        member this.Item with get ({ Class = tdef; Interface = impl }) = this.[tdef, impl]

[<Sealed>]
type InterfaceImplTableBuilder internal (owner: IndexOwner) =
    let mapping = Dictionary<SimpleIndex<TypeDefRow>, InterfaceIndexEntries>()
    let mutable count = 0

    member _.Count = count

    /// <param name="typeRow">The <c>TypeDef</c> that will implement the interface.</param>
    /// <param name="intf">The interface to implement.</param>
    /// <param name="duplicate">
    /// When this method returns, contains <see langword="true"/> if the type already implements the specified interface;
    /// otherwise, contains <see langword="false"/>.
    /// </param>
    member _.Add(typeRow, intf, duplicate: outref<bool>): unit =
        IndexOwner.checkIndex owner typeRow
        IndexOwner.checkOwner owner intf
        let impls =
            match mapping.TryGetValue typeRow with
            | (true, existing) -> existing
            | (false, _) ->
                let empty = InterfaceIndexEntries()
                mapping.[typeRow] <- empty
                empty
        count <- count + 1
        impls.Add(intf, &duplicate)

    member internal _.ToImmutable() =
        let rows = ImmutableArray.CreateBuilder<_> count
        let lookup = Dictionary<_, _> count
        for KeyValue(typeDef, entries) in mapping do
            let imap = Dictionary<_, int32> entries.Count
            for impl in entries do
                imap.[impl] <- rows.Count
                rows.Add { Class = typeDef; Interface = impl }
            lookup.[typeDef] <- imap :> IReadOnlyDictionary<_, _>
        InterfaceImplTable(rows.ToImmutable(), lookup)
