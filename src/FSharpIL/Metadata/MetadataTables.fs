﻿namespace rec FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Collections.ObjectModel
open System.Reflection

open FSharpIL.Metadata

[<RequireQualifiedAccess>]
module internal SystemType =
    let Delegate = "System", NonEmptyName "Delegate"
    let Enum = "System", NonEmptyName "Enum"
    let ValueType = "System", NonEmptyName "ValueType"

/// <summary>
/// Represents a violation of a Common Language Specification rule (I.7).
/// </summary>
type ClsViolation = // TODO: Rename to CLSViolation
    | PointerTypeUsage // of

type ValidationWarning =
    /// 10
    | DuplicateAssemblyRef of AssemblyRef
    /// 1d
    | TypeRefUsesModuleResolutionScope of TypeRef

type ValidationError =
    | DuplicateValue of IHandleValue
    | MissingType of ns: string * NonEmptyName

    override this.ToString() =
        match this with
        | DuplicateValue value -> sprintf "Cannot add duplicate %s (%A) to the table" (value.GetType().Name) value
        | MissingType(ns, name) ->
            match ns with
            | "" -> string name
            | _ -> sprintf "%s.%A" ns name
            |> sprintf "Unable to find type \"%s\", perhaps a TypeDef or TypeRef is missing"

type ValidationResult<'Result> = ValidationResult<'Result, ClsViolation, ValidationWarning, ValidationError>

type internal ITable<'Value> =
    inherit IReadOnlyCollection<'Value>

    abstract Comparer : IEqualityComparer<'Value>

type Table<'Value when 'Value :> IHandleValue> internal (state: MetadataBuilderState, comparer: IEqualityComparer<'Value>) =
    let set = HashSet<'Value> comparer

    new(state: MetadataBuilderState) = Table(state, EqualityComparer.Default)

    abstract member GetHandle : 'Value -> Result<Handle<'Value>, ValidationError>
    default _.GetHandle(value: 'Value) =
        state.EnsureOwner value
        if set.Add value |> not
        then value :> IHandleValue |> DuplicateValue |> Error
        else state.CreateHandle value |> Ok

    interface ITable<'Value> with
        member _.Comparer = comparer
        member _.Count = set.Count
        member _.GetEnumerator() = set.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = set.GetEnumerator() :> System.Collections.IEnumerator

/// II.22.30
type ModuleTable =
    { // Generation
      Name: NonEmptyName
      Mvid: Guid
      // EncId
      // EncBaseId
      }

    static member Default =
        { Name = NonEmptyName "Default.dll"
          Mvid = Guid.Empty } // TODO: What should the default Mvid be?

[<NoComparison; StructuralEquality>]
[<RequireQualifiedAccess>]
type ResolutionScope =
    | Module // NOTE: Does not occur in a compressed CLI module, and should produce a warning.
    | ModuleRef // of Handle<?>
    | AssemblyRef of Handle<AssemblyRef>
    | TypeRef of Handle<TypeRef>
    | Null

/// II.22.38
[<NoComparison; CustomEquality>]
type TypeRef =
    { ResolutionScope: ResolutionScope
      TypeName: NonEmptyName
      TypeNamespace: string }

    interface IEquatable<TypeRef> with
        member this.Equals other =
            this.ResolutionScope = other.ResolutionScope
            && this.TypeName = other.TypeName
            && this.TypeNamespace = other.TypeNamespace

    interface IHandleValue with
        member this.Handles =
            match this.ResolutionScope with
            | ResolutionScope.AssemblyRef (IHandle handle)
            | ResolutionScope.TypeRef (IHandle handle) -> handle |> Seq.singleton
            | _ -> Seq.empty

[<Sealed>]
type TypeRefTable internal (state: MetadataBuilderState) = // NOTE: First type in this should be the <Module> pseudo-class
    inherit Table<TypeRef>(state)

    let search = Dictionary<string * NonEmptyName, TypeRef> 8

    /// <summary>
    /// Searches for a type with the specified name and namespace, with a resolution scope
    /// of <see cref="FSharpIL.Metadata.ResolutionScope.AssemblyRef"/>.
    /// </summary>
    member internal this.FindType((ns, name) as t) =
        match search.TryGetValue(t) with
        | (true, existing) -> state.CreateHandle existing |> Some
        | (false, _) ->
            Seq.tryPick
                (function
                | { ResolutionScope = ResolutionScope.AssemblyRef _ } as t' when t'.TypeName = name && t'.TypeNamespace = ns ->
                    search.Item <- (ns, name), t'
                    state.CreateHandle t' |> Some
                | _ -> None)
                this

    override _.GetHandle typeRef =
        let token = base.GetHandle typeRef

        match token with
        | Ok _ ->
            // TODO: Check that the name is a "valid CLS identifier".

            match typeRef.ResolutionScope with
            | ResolutionScope.Module -> TypeRefUsesModuleResolutionScope typeRef |> state.Warnings.Add
            | _ -> ()
        | _ -> ()

        token

/// <summary>
/// Specifies which type a <see cref="FSharpIL.Metadata.TypeDef"/> extends.
/// </summary>
[<NoComparison; StructuralEquality>]
[<RequireQualifiedAccess>]
type Extends =
    | TypeDef of Handle<TypeDef>
    | TypeRef of Handle<TypeRef>
    // | TypeSpec of Handle<?>
    /// <summary>
    /// Indicates that a class does not extend another class, used by <see cref="System.Object"/> and interfaces.
    /// </summary>
    | Null

[<RequireQualifiedAccess>]
type TypeVisibility =
    | NotPublic
    | Public
    | NestedPublic of Handle<TypeDef>
    | NestedPrivate of Handle<TypeDef>
    /// <summary>Equivalent to the C# <see langword="protected"/> keyword.</summary>
    | NestedFamily of Handle<TypeDef>
    /// <summary>Equivalent to the C# <see langword="internal"/> keyword.</summary>
    | NestedAssembly of Handle<TypeDef>
    /// <summary>Equivalent to the C# <see langword="private protected"/> keyword.</summary>
    | NestedFamilyAndAssembly of Handle<TypeDef>
    /// <summary>Equivalent to the C# <see langword="protected internal"/> keyword.</summary>
    | NestedFamilyOrAssembly of Handle<TypeDef>

    /// <summary>Retrieves the enclosing class of this nested class.</summary>
    /// <remarks>In the actual metadata, nested type information is actually stored in the NestedClass table (II.22.32).</remarks>
    member this.EnclosingClass =
        match this with
        | NotPublic
        | Public -> None
        | NestedPublic parent
        | NestedPrivate parent
        | NestedFamily parent
        | NestedAssembly parent
        | NestedFamilyAndAssembly parent
        | NestedFamilyOrAssembly parent -> Some parent

    member this.Flags =
        match this with
        | NotPublic -> TypeAttributes.NotPublic
        | Public -> TypeAttributes.Public
        | NestedPublic _ -> TypeAttributes.NestedPublic
        | NestedPrivate _ -> TypeAttributes.NestedPrivate
        | NestedFamily _ -> TypeAttributes.NestedFamily
        | NestedAssembly _ -> TypeAttributes.NestedAssembly
        | NestedFamilyAndAssembly _ -> TypeAttributes.NestedFamANDAssem
        | NestedFamilyOrAssembly _ -> TypeAttributes.NestedFamORAssem

type ClassDef<'Flags, 'Method when 'Flags :> IFlags<TypeAttributes>> =
    { Access: TypeVisibility
      Flags: 'Flags
      ClassName: NonEmptyName
      TypeNamespace: string
      Extends: Extends // TODO: How to ensure that the base class is not sealed?
      FieldList: unit
      MethodList: 'Method }

type ConcreteClassDef = ClassDef<ClassFlags, unit>
type AbstractClassDef = ClassDef<AbstractClassFlags, unit>

/// <summary>
/// Defines a delegate type, which is a <see cref="FSharpIL.Metadata.TypeDef"/> that derives from <see cref="System.Delegate"/>.
/// </summary>
type DelegateDef =
    { Access: TypeVisibility
      Flags: DelegateFlags
      DelegateName: NonEmptyName
      TypeNamespace: string }

/// <summary>
/// Defines an enumeration type, which is a <see cref="FSharpIL.Metadata.TypeDef"/> that derives from <see cref="System.Enum"/>.
/// </summary>
type EnumDef =
  { Access: TypeVisibility
    EnumName: NonEmptyName
    TypeNamespace: string
    ValueList: unit }

type InterfaceDef =
    { Access: TypeVisibility
      Flags: InterfaceFlags
      InterfaceName: NonEmptyName
      TypeNamespace: string
      FieldList: unit // NOTE: Apparently static fields are allowed in interfaces?
      MethodList: unit }

/// <summary>
/// Defines a struct, which is a <see cref="FSharpIL.Metadata.TypeDef"/> that derives from <see cref="System.ValueType"/>.
/// </summary>
type StructDef =
   { Access: TypeVisibility
     Flags: StructFlags
     StructName: NonEmptyName
     TypeNamespace: string
     FieldList: unit
     MethodList: unit }

// TODO: Make this record a class instead to make the constructor internal?
/// <summary>
/// Represents a row in the <see cref="FSharpIL.Metadata.TypeDefTable"/> (II.22.37). Do not construct this type directly.
/// </summary>
/// <seealso cref="FSharpIL.Metadata.ClassDef"/>
/// <seealso cref="FSharpIL.Metadata.DelegateDef"/>
/// <seealso cref="FSharpIL.Metadata.EnumDef"/>
/// <seealso cref="FSharpIL.Metadata.InterfaceDef"/>
/// <seealso cref="FSharpIL.Metadata.StructDef"/>
[<CustomEquality; NoComparison>]
type TypeDef =
    { Flags: System.Reflection.TypeAttributes
      TypeName: NonEmptyName
      TypeNamespace: string
      Extends: Extends
      FieldList: unit
      MethodList: unit
      EnclosingClass: Handle<TypeDef> option }

    interface IEquatable<TypeDef> with
        member this.Equals other =
            this.TypeNamespace = other.TypeNamespace && this.TypeName = other.TypeName

    interface IHandleValue with
        member this.Handles =
            seq {
                match this.Extends with
                | Extends.TypeDef (IHandle handle)
                | Extends.TypeRef (IHandle handle) -> handle
                | Extends.Null -> ()

                match this.EnclosingClass with
                | Some(IHandle parent) -> parent
                | None -> ()
            }

[<Sealed>]
type TypeDefTable internal (owner: MetadataBuilderState) =
    let defs = Table<TypeDef> owner

    // TODO: Add the <Module> class used for global variables and functions.

    // TODO: Add functions for adding abstract classes and nested types.
    // TODO: Enforce CLS checks and warnings.
    member _.GetHandle({ Flags = flags } as def: ClassDef<_, _>) =
        { Flags = flags.Flags ||| def.Access.Flags
          TypeName = def.ClassName
          TypeNamespace = def.TypeNamespace
          Extends = def.Extends
          FieldList = ()
          MethodList = ()
          EnclosingClass = def.Access.EnclosingClass }
        |> defs.GetHandle

    member _.GetHandle({ Flags = DelegateFlags flags } as def: DelegateDef) =
        match owner.FindType SystemType.Delegate with
        | Some super ->
            { Flags = flags
              TypeName = def.DelegateName
              TypeNamespace = def.TypeNamespace
              Extends = Extends.TypeRef super
              FieldList = ()
              MethodList = ()
              EnclosingClass = def.Access.EnclosingClass }
            |> Ok
        | None -> MissingType SystemType.Delegate |> Error
        |> Result.bind defs.GetHandle

    member _.GetHandle(def: EnumDef) =
        match owner.FindType SystemType.Enum with
        | Some super ->
            { Flags = TypeAttributes.Sealed ||| TypeAttributes.Serializable
              TypeName = def.EnumName
              TypeNamespace = def.TypeNamespace
              Extends = Extends.TypeRef super
              FieldList = ()
              MethodList = ()
              EnclosingClass = def.Access.EnclosingClass }
            |> Ok
        | None -> MissingType SystemType.Enum |> Error
        |> Result.bind defs.GetHandle

    member _.GetHandle({ Flags = InterfaceFlags flags } as def: InterfaceDef) =
        { Flags = flags
          TypeName = def.InterfaceName
          TypeNamespace = def.TypeNamespace
          Extends = Extends.Null
          FieldList = ()
          MethodList = ()
          EnclosingClass = def.Access.EnclosingClass }
        |> defs.GetHandle

    member _.GetHandle({ Flags = StructFlags flags } as def: StructDef) =
        match owner.FindType SystemType.ValueType with
        | Some super ->
            { Flags = flags
              TypeName = def.StructName
              TypeNamespace = def.TypeNamespace
              Extends = Extends.TypeRef super
              FieldList = ()
              MethodList = ()
              EnclosingClass = def.Access.EnclosingClass }
            |> Ok
        | None -> MissingType SystemType.ValueType |> Error
        |> Result.bind defs.GetHandle

    interface ITable<TypeDef> with
        member _.Comparer = (defs :> ITable<_>).Comparer
        member _.Count = (defs :> ITable<_>).Count
        member _.GetEnumerator() = (defs :> IEnumerable<_>).GetEnumerator()
        member _.GetEnumerator() = (defs :> System.Collections.IEnumerable).GetEnumerator()

type Field<'Flags when 'Flags :> IFlags<FieldAttributes>> =
    { Flags: 'Flags
      Name: NonEmptyName
      Signature: unit }

/// <summary>
/// Represents a non-static <see cref="FSharpIL.Metadata.FieldRow"/>.
/// </summary>
type InstanceField = Field<InstanceFieldFlags>

type StaticField = Field<StaticFieldFlags>

/// <summary>
/// Represents a <see cref="FSharpIL.Metadata.FieldRow"/> defined inside of the `<Module>` pseudo-class.
/// </summary>
type GlobalField = unit

/// <summary>Represents a row in the Field table (II.22.15).</summary>
[<Sealed>]
type FieldRow internal (flags: FieldAttributes, name: NonEmptyName, signature: unit) =
    member val Flags = flags
    member val Name = name
    member val Signature = signature

    interface IEquatable<FieldRow> with
        member this.Equals other =
            if this.Flags &&& FieldAttributes.FieldAccessMask = FieldAttributes.PrivateScope
            then false
            else
                this.Name = other.Name && this.Signature = other.Signature

/// <summary>Represents a set of fields owned by a <see cref="FSharpIL.Metadata.FieldRow"/>.</summary>
[<Sealed>]
type FieldSet<'Flags when 'Flags :> IFlags<FieldAttributes>> (capacity: int) =
    let fields = HashSet<FieldRow> capacity

    new() = FieldSet(1)

    member _.Add(field: Field<'Flags>) =
        let field' = FieldRow(field.Flags.Flags, field.Name, field.Signature)
        if fields.Add field'
        then Ok()
        else Error field

    member _.ToImmutable() = fields.ToImmutableArray()

/// II.22.2
type Assembly =
    { HashAlgId: unit // II.23.1.1
      Version: Version
      Flags: unit
      PublicKey: unit option
      Name: AssemblyName
      Culture: AssemblyCulture }

/// II.22.5
[<CustomEquality; NoComparison>]
type AssemblyRef =
    { Version: Version
      Flags: unit
      PublicKeyOrToken: PublicKeyOrToken
      Name: AssemblyName
      Culture: AssemblyCulture
      HashValue: unit option }

    interface IEquatable<AssemblyRef> with
        member this.Equals other =
            this.Version = other.Version
            && this.PublicKeyOrToken = other.PublicKeyOrToken
            && this.Name = other.Name
            && this.Culture = other.Culture

    interface IHandleValue with member _.Handles = Seq.empty

[<Sealed>]
type AssemblyRefTable internal (state: MetadataBuilderState) =
    let set = HashSet()

    member _.GetHandle assemblyRef =
        if set.Add assemblyRef |> not then
            DuplicateAssemblyRef assemblyRef |> state.Warnings.Add
        state.CreateHandle assemblyRef

    interface ITable<AssemblyRef> with
        member _.Comparer = EqualityComparer<_>.Default :> IEqualityComparer<_>
        member _.Count = set.Count
        member _.GetEnumerator() = set.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = set.GetEnumerator() :> System.Collections.IEnumerator

// II.22.32
[<Struct; System.Runtime.CompilerServices.IsReadOnly>]
type NestedClass =
    { NestedClass: Handle<TypeDef>
      EnclosingClass: Handle<TypeDef> }

[<Sealed>]
type MetadataBuilderState () as this =
    let owner = Object()

    let typeDef = TypeDefTable this

    member val internal Warnings: ImmutableArray<_>.Builder = ImmutableArray.CreateBuilder<ValidationWarning>()
    member val internal ClsViolations: ImmutableArray<_>.Builder = ImmutableArray.CreateBuilder<ClsViolation>()

    // Reserved: uint32
    member val MajorVersion: byte = 2uy
    member val MinorVersion: byte = 0uy
    // HeapSizes: byte
    // Reserved: byte
    // Valid: uint64
    // Sorted: uint64 // TODO: Figure out what Sorted is used for.
    // Rows
    /// (0x00)
    member val Module = ModuleTable.Default with get, set
    /// (0x01)
    member val TypeRef = TypeRefTable this
    /// (0x02)
    member _.TypeDef = typeDef
    // (0x04)
    // Field

    /// (0x20)
    member val Assembly: Assembly option = None with get, set // 0x20 // TODO: Figure out if None is a good default value.
    // AssemblyProcessor // 0x21 // Not used when writing a PE file
    // AssemblyOS // 0x22 // Not used when writing a PE file
    /// (0x23)
    member val AssemblyRef = AssemblyRefTable this

    /// (0x29)
    member val NestedClass =
        Seq.choose
            (function
            | { TypeDef.EnclosingClass = Some parent } as tdef ->
                { NestedClass = this.CreateHandle tdef
                  EnclosingClass = parent }
                |> Some
            | _ -> None)
            typeDef

    member internal this.FindType t =
        // TODO: Search in the TypeDefTable as well.
        this.TypeRef.FindType t

    member internal _.CreateHandle<'T> (value: 'T): Handle<'T> = Handle(owner, value)

    member internal this.CreateTable<'T> (table: ITable<'T>): ReadOnlyDictionary<Handle<'T>, uint32> =
        let mutable i = 0u
        let dict = Dictionary<_, _>(table.Count, HandleEqualityComparer table.Comparer)
        for value in table do
            dict.Item <- this.CreateHandle value, i
            i <- i + 1u
        ReadOnlyDictionary dict

    member internal _.EnsureOwner(value: IHandleValue) =
        for handle in value.Handles do
            if handle.Owner <> owner then
                sprintf
                    "A handle to a %s owned by another state was incorrectly referenced by an %s."
                    handle.ValueType.Name
                    (value.GetType().Name)
                |> invalidArg "item"
