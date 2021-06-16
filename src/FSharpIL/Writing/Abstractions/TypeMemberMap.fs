namespace FSharpIL.Writing.Abstractions

open System.Collections
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata.Tables
open FSharpIL.Writing
open FSharpIL.Writing.Tables
open FSharpIL.Writing.Tables.Collections

[<Struct>]
type MemberSet<'Member when 'Member : struct and 'Member :> ITableRow> =
    private { mutable Members: HashSet<'Member> }

    member this.Count = if this.Members = null then 0 else this.Members.Count
    member this.IsEmpty = this.Count = 0

    member this.Contains(row: inref<'Member>) =
        if this.Members = null
        then false
        else this.Members.Contains row

    member internal this.Add(row: inref<'Member>) =
        if this.Members = null then this.Members <- HashSet()
        this.Members.Add row

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
        member this.Contains item = this.Contains &item
        member this.IsProperSubsetOf other = if this.Members = null then not(Seq.isEmpty other) else this.Members.IsProperSubsetOf other
        member this.IsProperSupersetOf other = if this.Members = null then false else this.Members.IsProperSupersetOf other
        member this.IsSubsetOf other = if this.Members = null then true else this.Members.IsSubsetOf other
        member this.IsSupersetOf other = if this.Members = null then Seq.isEmpty other else this.Members.IsSupersetOf other
        member this.Overlaps other = if this.Members = null then false else this.Members.Overlaps other
        member this.SetEquals other = if this.Members = null then Seq.isEmpty other else this.Members.SetEquals other
#endif

[<Struct>]
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

    // TODO: get inref to existing entry instead?
    let inline findMembers typeDefIndex (members: TypeMemberMap) = members.[typeDefIndex]

    let inline private trySerizalizeMembers builder (members: MemberSet<'Row>) =
        match (^Builder : (member TryAdd : ImmutableArray<'Row> -> ValidationResult<TableIndexRange<'Row>>) (builder, (members.ToImmutableArray()))) with
        | Ok _ -> None
        | Error err -> Some err

    let trySerialize (builder: MetadataTablesBuilder) { MemberMap = members } =
        Seq.tryPick
            (fun index ->
                let members' = &members.ValueRef index
                match trySerizalizeMembers builder.Field members'.Fields with
                | None ->
                    // TODO: Also add properties and events
                    trySerizalizeMembers builder.MethodDef members'.Methods
                | err -> err)
            members.Keys

    /// <exception cref="T:FSharpIL.Writing.Tables.ValidationErrorException">
    /// Thrown when a row could not be added to the <c>Field</c>, <c>MethodDef</c>, <c>Event</c>, or <c>Property</c> tables.
    /// </exception>
    let serialize builder members =
        match trySerialize builder members with
        | None -> ()
        | Some err -> ValidationError.throw err
