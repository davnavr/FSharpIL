namespace FSharpIL.Writing.Abstractions

open System.Collections
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata.Tables
open FSharpIL.Writing

[<IsReadOnly; Struct>]
type MemberSet<'Member when 'Member : struct and 'Member :> ITableRow> =
    private { Members: HashSet<'Member> }

    member this.Count = if this.Members = null then 0 else this.Members.Count
    member this.IsEmpty = this.Count = 0

    member this.Contains row =
        if this.Members = null
        then false
        else this.Members.Contains row

    member this.GetEnumerator() =
        if this.Members = null
        then Seq.empty.GetEnumerator()
        else this.Members.GetEnumerator() :> IEnumerator<_>

    interface IReadOnlyCollection<'Member> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator()
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator

#if NET5_0
    interface IReadOnlySet<'Member> with
        member this.Contains item = this.Contains item
        member this.IsProperSubsetOf other = if this.Members = null then not(Seq.isEmpty other) else this.Members.IsProperSubsetOf other
        member this.IsProperSupersetOf other = if this.Members = null then false else this.Members.IsProperSupersetOf other
        member this.IsSubsetOf other = if this.Members = null then true else this.Members.IsSubsetOf other
        member this.IsSupersetOf other = if this.Members = null then Seq.isEmpty other else this.Members.IsSupersetOf other
        member this.Overlaps other = if this.Members = null then false else this.Members.Overlaps other
        member this.SetEquals other = if this.Members = null then Seq.isEmpty other else this.Members.SetEquals other
#endif

[<IsReadOnly; Struct>]
[<NoComparison; NoEquality>]
type TypeMembers =
    { Fields: MemberSet<FieldRow>
      Methods: MemberSet<MethodDefRow>
      Events: MemberSet<EventRow>
      Properties: MemberSet<PropertyRow> }

/// Keeps track of which fields, methods, events, and properties are associated with which types.
[<NoComparison; NoEquality>]
type TypeMemberMap =
    internal
        { // TODO: Figure out how to force addition of <Module> row, maybe use TypeDefRow instead of TableIndex<TypeDefRow> as key?
          // TODO: Figure out how to avoid allocation of nodes in sorted dictionary, maybe keep track of indices in a separate HashSet?
          MemberMap: ImmutableSortedDictionary<TableIndex<TypeDefRow>, TypeMembers>.Builder }

    member this.Item with get typeDefIndex =
        match this.MemberMap.TryGetValue typeDefIndex with
        | true, existing -> existing
        | false, _ -> Unchecked.defaultof<_>

    interface IReadOnlyDictionary<TableIndex<TypeDefRow>, TypeMembers> with
        member this.Count = this.MemberMap.Count
        member this.Item with get key = this.[key]
        member this.Keys = this.MemberMap.Keys
        member this.Values = this.MemberMap.Values
        member _.ContainsKey _ = true
        member this.TryGetValue(key, value: byref<_>) =
            if not(this.MemberMap.TryGetValue(key, &value)) then value <- Unchecked.defaultof<_>
            true
        member this.GetEnumerator() = this.MemberMap.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.MemberMap.GetEnumerator() :> IEnumerator

[<RequireQualifiedAccess>]
module TypeMemberMap =
    let empty() = { MemberMap = ImmutableSortedDictionary.CreateBuilder() }

    let inline findMembers typeDefIndex (members: TypeMemberMap) = members.[typeDefIndex]

    let addTableRows (builder: MetadataTablesBuilder) { MemberMap = members } =
        for index in members.Keys do
            let members' = &members.ValueRef index
            builder.Field.TryAdd(members'.Fields.ToImmutableArray())
            ()
        failwith "TODO: Add it"
