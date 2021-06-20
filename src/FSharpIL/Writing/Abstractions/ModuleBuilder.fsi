namespace FSharpIL.Writing.Abstractions

open System
open System.Collections.Immutable
open System.Collections.ObjectModel
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.Writing
open FSharpIL.Writing.Tables

[<IsReadOnly; Struct>]
type TypeEntryIndex<'Tag> = internal { TypeEntry: int32 }

[<IsReadOnly; Struct>]
type MemberEntryIndex<'Tag> = internal { Owner: TypeEntryIndex<unit>; MemberEntry: int32 }
type MemberEntryIndex<'Tag> with
    member OwnerIndex : TypeEntryIndex<unit>

[<IsReadOnly; Struct>]
type MethodEntry =
    internal
        { Body: MethodBodyLocation
          ImplFlags: MethodImplFlags
          Flags: MethodDefFlags
          MethodName: StringOffset
          Signature: FSharpIL.Metadata.Blobs.MethodDefSigOffset
          ParamList: ImmutableArray<ParamRow> }

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type internal TypeEntry =
    { Flags: TypeDefFlags
      TypeName: StringOffset
      TypeNamespace: StringOffset
      Extends: TypeDefOrRef // TODO: Fix, Extends should use TypeEntryIndex instead.
      EnclosingClass: TypeEntryIndex<unit> voption }

    interface IEquatable<TypeEntry>
type internal TypeEntry with
    override Equals : obj -> bool
    override GetHashCode : unit -> int32
    member Equals : other: TypeEntry -> bool

[<Struct>]
[<NoComparison; NoEquality>]
type MemberList<'Member when 'Member : struct> =
    private { mutable Members: ImmutableArray<'Member>.Builder }
    interface System.Collections.Generic.IReadOnlyCollection<'Member>
type MemberList<'Member when 'Member : struct> with
    member Count : int32
    member IsEmpty : bool
    member internal Add : inref<'Member> -> unit
    member internal ToImmutable: unit -> ImmutableArray<'Member>

[<Struct>]
[<NoComparison; NoEquality>]
type TypeMembers =
    { Fields: MemberList<FieldRow>
      Methods: MemberList<MethodEntry>
      Events: MemberList<EventRow>
      Properties: MemberList<PropertyRow> }

[<NoComparison; NoEquality>]
type ModuleBuilder =
    internal
        { Metadata: CliMetadataBuilder
          Types: ImmutableArray<TypeEntry>.Builder
          MemberMap: ImmutableSortedDictionary<TypeEntryIndex<unit>, TypeMembers>.Builder
          [<DefaultValue>] mutable GlobalMembers: TypeMembers }

    member Strings : StringsStreamBuilder
    member UserString : UserStringStreamBuilder
    member Guid : GuidStreamBuilder
    member Blob : BlobStreamBuilder

    member Globals : inref<TypeMembers>
    member Assembly : AssemblyRow voption

[<RequireQualifiedAccess>]
module TypeEntryIndex =
    val removeTag : index: TypeEntryIndex<'Tag> -> TypeEntryIndex<unit>

[<RequireQualifiedAccess>]
module ModuleBuilder = // TODO: Replace public and internal functions with instance methods (except for create functions).
    val internal addTypeEntry: entry: inref<TypeEntry> -> builder: ModuleBuilder -> TypeEntryIndex<unit>

    val internal addMethodEntry:
        owner: TypeEntryIndex<unit> ->
        entry: inref<MethodEntry> ->
        builder: ModuleBuilder ->
        MemberEntryIndex<MethodEntry>

    val create:
        moduleRow: RowBuilder<ModuleRow> ->
        assemblyRow: RowBuilder<AssemblyRow> voption ->
        header: CliHeader ->
        root: CliMetadataRoot<FSharpIL.Omitted, FSharpIL.Omitted> ->
        struct(ModuleBuilder * TableIndex<AssemblyRow> voption)

    val createAssembly:
        moduleRow: RowBuilder<ModuleRow> ->
        assemblyRow: RowBuilder<AssemblyRow> ->
        header: CliHeader ->
        root: CliMetadataRoot<FSharpIL.Omitted, FSharpIL.Omitted> ->
        struct(ModuleBuilder * TableIndex<AssemblyRow>)

    val addAssemblyRef:
        version: Version ->
        publicKeyOrToken: FSharpIL.Metadata.Blobs.PublicKeyOrToken ->
        name: AssemblyName ->
        culture: Identifier voption ->
        hashValue: BlobOffset ->
        builder: ModuleBuilder ->
        TableIndex<AssemblyRefRow>

    val tryGetAssembly: assembly: outref<AssemblyRow> -> builder: ModuleBuilder -> bool

    val trySerializeRows:
        builder: ModuleBuilder ->
        ValidationResult<struct(CliMetadataBuilder * ReadOnlyDictionary<TypeEntryIndex<unit>, TableIndex<TypeDefRow>>)>
