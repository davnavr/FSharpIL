namespace FSharpIL.Writing.Abstractions // TODO: Make an FSharpIL.Cli namespace

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.Writing
open FSharpIL.Writing.Tables

[<IsReadOnly; Struct>]
type TypeEntryIndex<'Tag> = internal { TypeEntry: int32 }

[<IsReadOnly; Struct>]
type MemberEntryIndex<'Tag> =
    internal { Owner: TypeEntryIndex<unit>; MemberEntry: int32 }
    member this.OwnerIndex = this.Owner

[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type internal TypeEntry = // TODO: Rename to TypeDefinition
    { Flags: TypeDefFlags
      TypeName: IdentifierOffset
      TypeNamespace: StringOffset
      Extends: TypeDefOrRef // TODO: Fix, Extends should use TypeEntryIndex instead.
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

[<IsReadOnly; Struct>]
type MethodEntry = // TODO: Rename to MethodDefinition
    internal
        { Body: MethodBodyLocation
          ImplFlags: MethodImplFlags
          Flags: MethodDefFlags
          MethodName: IdentifierOffset
          Signature: FSharpIL.Metadata.Blobs.MethodDefSigOffset
          ParamList: ImmutableArray<ParamRow> }

[<Struct>]
[<NoComparison; NoEquality>]
type MemberList<'Member when 'Member : struct> =
    private { mutable Members: ImmutableArray<'Member>.Builder }

    member this.Count = if this.Members = null then 0 else this.Members.Count
    member this.IsEmpty = this.Count = 0

    member this.Add(row: inref<'Member>) =
        if this.Members = null then this.Members <- ImmutableArray.CreateBuilder()
        this.Members.Add row

    member this.ToImmutable() = if this.IsEmpty then ImmutableArray.Empty else this.Members.ToImmutable()

    interface IReadOnlyCollection<'Member> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.Members.GetEnumerator()
        member this.GetEnumerator() = this.Members.GetEnumerator() :> IEnumerator

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

    member this.Strings = this.Metadata.Strings
    member this.UserString = this.Metadata.UserString
    member this.Guid = this.Metadata.Guid
    member this.Blob = this.Metadata.Blob

    member this.Globals: inref<_> = &this.GlobalMembers
    member this.Assembly =
        match this.Metadata.Tables.Assembly.TryGetRow() with
        | true, row -> ValueSome row
        | false, _ -> ValueNone

[<RequireQualifiedAccess>]
module TypeEntryIndex =
    let removeTag { TypeEntry = entry }: TypeEntryIndex<unit> = { TypeEntry = entry }

[<RequireQualifiedAccess>]
module ModuleBuilder =
    let [<Literal>] DefaultTypeCapacity = 64

    let moduleTypeName = Identifier.ofStr "<Module>"

    let create moduleRow (assemblyRow: RowBuilder<AssemblyRow> voption) header root =
        let builder =
            { Metadata = CliMetadataBuilder(moduleRow, header, root)
              Types = ImmutableArray.CreateBuilder DefaultTypeCapacity
              MemberMap = ImmutableSortedDictionary.CreateBuilder() }

        let assemblyRowIndex =
            match assemblyRow with
            | ValueSome assemblyRowBuilder ->
                let row = assemblyRowBuilder builder.Strings builder.Guid builder.Blob
                ValueSome(builder.Metadata.Tables.Assembly.SetRow &row)
            | ValueNone -> ValueNone

        struct(builder, assemblyRowIndex)

    let createAssembly moduleRow assemblyRow header root =
        let struct(builder, assemblyRowIndex) = create moduleRow (ValueSome assemblyRow) header root
        struct(builder, assemblyRowIndex.Value)

    let tryGetAssembly (assembly: outref<_>) { Metadata = builder } = builder.Tables.Assembly.TryGetRow &assembly

    let addTypeEntry (entry: inref<TypeEntry>) builder: TypeEntryIndex<unit> =
        builder.Types.Add entry
        { TypeEntry = builder.Types.Count }

    type IEntryMembers<'Member when 'Member : struct> = interface
        abstract Members : members: inref<TypeMembers> -> MemberList<'Member>
    end

    let addMemberEntry<'List, 'Member when 'List :> IEntryMembers<'Member> and 'List : struct and 'Member : struct>
        owner
        (entry: inref<'Member>)
        builder
        =
        let mutable members = builder.MemberMap.GetValueOrDefault owner
        let entries = Unchecked.defaultof<'List>.Members &members
        let index: MemberEntryIndex<'Member> = { Owner = owner; MemberEntry = entries.Count }
        entries.Add &entry
        builder.MemberMap.[owner] <- members
        index

    type [<Struct>] AddMethodEntry = interface IEntryMembers<MethodEntry> with member _.Members members = members.Methods

    let addMethodEntry owner (entry: inref<_>) builder = addMemberEntry<AddMethodEntry, _> owner &entry builder

    [<RequireQualifiedAccess>]
    module MemberList =
        [<Interface>]
        type private IMemberSerializer<'Member, 'Row
            when 'Member : struct
            and 'Row :> ITableRow
            and 'Row : struct>
            =
            abstract Serialize: tables: MetadataTablesBuilder * members: ImmutableArray<'Member> -> ValidationResult<FSharpIL.Writing.Tables.Collections.TableIndexRange<'Row>>

        let inline private trySerialize<'Serializer, 'Member, 'Row
            when 'Serializer :> IMemberSerializer<'Member, 'Row>
            and 'Serializer : struct>
            (members: MemberList<'Member>)
            (counter: byref<uint32>)
            tables
            =
            let members' = members.ToImmutable()
            match Unchecked.defaultof<'Serializer>.Serialize(tables, members') with
            | Ok _ ->
                counter <- counter + uint32 members'.Length
                Ok()
            | Error err -> Error err

        let rec private trySerializeMethodsLoop
            (tables: MetadataTablesBuilder)
            (methods: ImmutableArray<_>)
            (rows: byref<MethodDefRow[]>)
            i
            =
            if i < rows.Length then
                let method = &methods.ItemRef i
                match tables.Param.TryAdd method.ParamList with
                | Ok paramRowList ->
                    rows.[i] <-
                        { Rva = method.Body
                          ImplFlags = method.ImplFlags
                          Flags = method.Flags
                          Name = method.MethodName
                          Signature = method.Signature
                          ParamList = paramRowList.StartIndex }
                    trySerializeMethodsLoop tables methods &rows (i + 1)
                | Error err -> Error err
            else Ok()

        type private MethodSerializer = struct
            interface IMemberSerializer<MethodEntry, MethodDefRow> with
                member _.Serialize(tables, methods) =
                    let mutable rows = Array.zeroCreate methods.Length
                    match trySerializeMethodsLoop tables methods &rows 0 with
                    | Ok() -> tables.MethodDef.TryAdd(Unsafe.As &rows)
                    | Error err -> Error err
        end

        let trySerializeMethods methods (counter: byref<_>) tables = trySerialize<MethodSerializer, _, _> methods &counter tables

    [<IsByRefLike; Struct>]
    type MemberCounter =
        [<DefaultValue>] val mutable FieldList: uint32
        [<DefaultValue>] val mutable MethodList: uint32

    let trySerializeMembers (members: inref<TypeMembers>) (counter: byref<MemberCounter>) tables =
        MemberList.trySerializeMethods members.Methods &counter.MethodList tables

    let tryAddNestedClass nested enclosing (lookup: Dictionary<_, _>) (table: NestedClassTableBuilder) =
        match enclosing with
        | ValueNone -> None
        | ValueSome(enclosing: TypeEntryIndex<unit>) ->
            let row =
                { NestedClass = nested
                  EnclosingClass = lookup.[enclosing] }
            ValidationError.toOption (table.TryAdd &row)

    let rec trySerializeTypes builder (lookup: Dictionary<_, _>) (counter: byref<MemberCounter>) i =
        if i < builder.Types.Count then
            let entry = &builder.Types.ItemRef i
            let row =
                { Flags = entry.Flags
                  TypeName = entry.TypeName
                  TypeNamespace = entry.TypeNamespace
                  Extends = failwith "TODO: Translate Extends"
                  FieldList = { TableIndex = counter.FieldList }
                  MethodList = { TableIndex = counter.MethodList } }

            match builder.Metadata.Tables.TypeDef.TryAddRow &row with
            | Ok row' ->
                lookup.[{ TypeEntry = i }] <- row'
                match trySerializeMembers (&builder.MemberMap.ValueRef { TypeEntry = i }) &counter builder.Metadata.Tables with
                | Ok _ ->
                    match tryAddNestedClass row' entry.EnclosingClass lookup builder.Metadata.Tables.NestedClass with
                    | None -> trySerializeTypes builder lookup &counter (i + 1)
                    | Some err -> Error err
                | Error err -> Error err
            | Error err -> Error err
        else Ok()

    let trySerializeRows builder =
        let tables = builder.Metadata.Tables
        let moduleTypeDef =
            { Flags = Unchecked.defaultof<TypeDefFlags>
              TypeName = builder.Strings.Add moduleTypeName
              TypeNamespace = { StringOffset = 0u }
              Extends = Unchecked.defaultof<TypeDefOrRef>
              FieldList = { TableIndex = 0u }
              MethodList = { TableIndex = 0u } }

        match tables.TypeDef.TryAddRow &moduleTypeDef with
        | Ok _ ->
            let mutable counter = MemberCounter()
            match trySerializeMembers &builder.Globals &counter tables with
            | Ok() ->
                let lookup = Dictionary builder.Types.Count
                match trySerializeTypes builder lookup &counter 0 with
                | Ok() -> Ok(struct(builder.Metadata, System.Collections.ObjectModel.ReadOnlyDictionary lookup))
                | Error err -> Error err
            | Error err -> Error err
        | Error err  -> Error err

    let referenceAssembly
        (version: Version)
        publicKeyOrToken
        (name: FileName)
        (culture: Identifier voption)
        hashValue
        (builder: ModuleBuilder)
        =
        let row =
            { MajorVersion = Checked.uint16 version.Major
              MinorVersion = Checked.uint16 version.Minor
              BuildNumber = Checked.uint16 version.Build
              RevisionNumber = Checked.uint16 version.Revision
              PublicKeyOrToken = publicKeyOrToken
              Name = builder.Strings.Add name
              Culture = builder.Strings.Add culture
              HashValue = hashValue }
        builder.Metadata.Tables.AssemblyRef.Add &row

    [<System.Obsolete>]
    let referenceType resolutionScope (typeName: Identifier) (typeNamespace: Identifier voption) (builder: ModuleBuilder) =
        let row =
            { ResolutionScope = resolutionScope
              TypeName = builder.Strings.Add typeName
              TypeNamespace = builder.Strings.Add typeNamespace } // TODO: Figure out how to handle warnings.
        builder.Metadata.Tables.TypeRef.TryAdd(&row, null)
