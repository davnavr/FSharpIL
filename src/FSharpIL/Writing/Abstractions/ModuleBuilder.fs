namespace FSharpIL.Writing.Abstractions

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Utilities

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.Writing

[<Struct>]
type internal MemberSet<'Member when 'Member : struct and 'Member : equality> =
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

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type internal MethodEntry =
    { Body: MethodBodyLocation
      ImplFlags: MethodImplFlags
      Flags: MethodDefFlags
      MethodName: StringOffset
      Signature: FSharpIL.Metadata.Blobs.MethodDefSigOffset
      ParamList: ImmutableArray<ParamRow> }

    member this.Equals other =
        if (this.Flags &&& other.Flags &&& MethodDefFlags.MemberAccessMask) = MethodDefFlags.CompilerControlled
        then false
        else
            this.MethodName = other.MethodName
            && this.ParamList.Length = other.ParamList.Length
            && this.Signature = other.Signature

    override this.GetHashCode() = HashCode.Combine(this.MethodName, this.ParamList, this.Signature)

    override this.Equals obj =
        match obj with
        | :? MethodEntry as other -> this.Equals other
        | _ -> false

    interface IEquatable<MethodEntry> with member this.Equals other = this.Equals other

[<Struct>]
[<NoComparison; NoEquality>]
type internal TypeMembers =
    { Fields: MemberSet<FieldRow>
      Methods: MemberSet<MethodEntry>
      Events: MemberSet<EventRow>
      Properties: MemberSet<PropertyRow> }

type [<IsReadOnly; Struct>] TypeEntryIndex<'Tag> = internal { TypeEntry: int32 }

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type internal TypeEntry =
    { Flags: TypeDefFlags
      TypeName: StringOffset
      TypeNamespace: StringOffset
      Extends: TypeDefOrRef
      EnclosingClass: TypeEntryIndex<unit> voption }

    member this.Equals other =
        this.TypeName = other.TypeName
        && this.TypeNamespace = other.TypeNamespace
        && this.EnclosingClass = other.EnclosingClass

    override this.GetHashCode() = HashCode.Combine(this.TypeName, this.TypeNamespace, this.EnclosingClass)

    override this.Equals obj =
        match obj with
        | :? TypeEntry as other -> this.Equals other
        | _ -> false

    interface IEquatable<TypeEntry> with member this.Equals other = this.Equals other

[<NoComparison; NoEquality>]
type ModuleBuilder =
    internal
        { Metadata: CliMetadataBuilder
          Types: ImmutableSortedDictionary<TypeEntry, TypeEntryIndex<unit>>.Builder // TODO: How are types sorted? Make TypeEntry implement IComparable?
          MemberMap: ImmutableSortedDictionary<TypeEntryIndex<unit>, TypeMembers>.Builder }

    member this.Strings = this.Metadata.Strings
    member this.UserString = this.Metadata.UserString
    member this.Guid = this.Metadata.Guid
    member this.Blob = this.Metadata.Blob
    member internal this.Globals = this.MemberMap.ValueRef { TypeEntry = 0 }

[<RequireQualifiedAccess>]
module TypeEntryIndex =
    let removeTag { TypeEntry = entry }: TypeEntryIndex<unit> = { TypeEntry = entry }

[<RequireQualifiedAccess>]
module ModuleBuilder =
    let private moduleTypeName = Identifier.ofStr "<Module>"

    let create moduleRow header root =
        let builder =
            { Metadata = CliMetadataBuilder(moduleRow, header, root)
              Types = ImmutableSortedDictionary.CreateBuilder()
              MemberMap = ImmutableSortedDictionary.CreateBuilder() }

        builder.Types.Add (
            { Flags = Unchecked.defaultof<_>
              TypeName = builder.Strings.Add moduleTypeName
              TypeNamespace = Unchecked.defaultof<StringOffset>
              Extends = Unchecked.defaultof<TypeDefOrRef>
              EnclosingClass = ValueNone },
            { TypeEntry = 0 }
        )

        failwith "TODO: Add <Module> type."
        builder

    let internal tryAddType (entry: inref<TypeEntry>) builder =
        builder.Types.TryAdd(entry, { TypeEntry = builder.Types.Count })
        // TODO: Return ValidationResult

    let serialize builder =
        failwith "TODO: Add member rows"
        builder.Metadata
