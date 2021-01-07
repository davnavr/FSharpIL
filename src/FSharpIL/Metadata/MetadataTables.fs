namespace rec FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable

[<AutoOpen>]
module internal Helpers =
    let inline (|Handle|) (handle: #IHandle) = handle :> IHandle

[<RequireQualifiedAccess>]
module internal SystemType =
    let Delegate = "System", NonEmptyName "Delegate"
    let Enum = "System", NonEmptyName "Enum"
    let ValueType = "System", NonEmptyName "ValueType"

/// <summary>
/// Represents a violation of a Common Language Specification rule (I.7).
/// </summary>
type ClsCheck = // TODO: Rename to CLSViolation
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

type ValidationResult<'Result> = ValidationResult<'Result, ClsCheck, ValidationWarning, ValidationError>

type IHandle =
    abstract Owner : MetadataBuilderState
    abstract ValueType : Type

type IHandleValue =
    abstract Handles: seq<IHandle>

/// <summary>
/// Guarantees that values originate from the same <see cref="FSharpIL.Metadata.MetadataBuilderState"/>.
/// </summary>
type Handle<'Value> =
    private
    | Handle of MetadataBuilderState * 'Value

    member this.Item = let (Handle (_, item)) = this in item

    interface IHandle with
        member this.Owner = let (Handle (owner, _)) = this in owner
        member this.ValueType = this.Item.GetType()

// TODO: Create new handle type containing an index and the item?

type Table<'Value when 'Value :> IHandleValue> internal (owner: MetadataBuilderState, comparer: IEqualityComparer<'Value>) =
    let set = ImmutableHashSet.CreateBuilder<'Value> comparer

    new(owner: MetadataBuilderState) = Table(owner, EqualityComparer.Default)

    member _.ToImmutable() = set.ToImmutable()

    abstract member GetToken : 'Value -> Result<Handle<'Value>, ValidationError>
    default _.GetToken(value: 'Value) =
        owner.EnsureOwner value
        if set.Add value |> not
        then value :> IHandleValue |> DuplicateValue |> Error
        else Handle (owner, value) |> Ok

    interface IEnumerable<'Value> with
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
            | ResolutionScope.AssemblyRef (Handle handle)
            | ResolutionScope.TypeRef (Handle handle) -> handle |> Seq.singleton
            | _ -> Seq.empty

[<Sealed>]
type TypeRefTable internal (owner: MetadataBuilderState) = // NOTE: First type in this should be the <Module> pseudo-class
    inherit Table<TypeRef>(owner)

    let search = Dictionary<string * NonEmptyName, TypeRef> 8

    /// <summary>
    /// Searches for a type with the specified name and namespace, with a resolution scope
    /// of <see cref="FSharpIL.Metadata.ResolutionScope.AssemblyRef"/>.
    /// </summary>
    member internal this.FindType((ns, name) as t) =
        match search.TryGetValue(t) with
        | (true, existing) -> Handle(owner, existing) |> Some
        | (false, _) ->
            Seq.tryPick
                (function
                | { ResolutionScope = ResolutionScope.AssemblyRef _ } as t' when t'.TypeName = name && t'.TypeNamespace = ns ->
                    search.Item <- (ns, name), t'
                    Handle(owner, t') |> Some
                | _ -> None)
                this

    override _.GetToken typeRef =
        let token = base.GetToken typeRef

        match token with
        | Ok _ ->
            // TODO: Check that the name is a "valid CLS identifier".

            match typeRef.ResolutionScope with
            | ResolutionScope.Module -> TypeRefUsesModuleResolutionScope typeRef |> owner.Warnings.Add
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
type TypeAccess =
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
        | NotPublic -> 0
        | Public -> 1
        | NestedPublic _ -> 2
        | NestedPrivate _ -> 3
        | NestedFamily _ -> 4
        | NestedAssembly _ -> 5
        | NestedFamilyAndAssembly _ -> 6
        | NestedFamilyOrAssembly _ -> 7
        |> enum<System.Reflection.TypeAttributes>

type ClassDef =
    { Access: TypeAccess
      Flags: ClassFlags
      ClassName: NonEmptyName
      TypeNamespace: string
      Extends: Extends
      FieldList: unit
      MethodList: unit }

// TODO: Create types for abstract classes and nested types. Maybe make the 5 types generic, with the parameter being an access modifier?

/// <summary>
/// Defines a delegate type, which is a <see cref="FSharpIL.Metadata.TypeDef"/> that derives from <see cref="System.Delegate"/>.
/// </summary>
type DelegateDef =
    { Access: TypeAccess
      Flags: DelegateFlags
      DelegateName: NonEmptyName
      TypeNamespace: string }

/// <summary>
/// Defines an enumeration type, which is a <see cref="FSharpIL.Metadata.TypeDef"/> that derives from <see cref="System.Enum"/>.
/// </summary>
type EnumDef =
  { Access: TypeAccess
    EnumName: NonEmptyName
    TypeNamespace: string
    ValueList: unit }

type InterfaceDef =
    { Access: TypeAccess
      Flags: InterfaceFlags
      InterfaceName: NonEmptyName
      TypeNamespace: string
      FieldList: unit // NOTE: Apparently static fields are allowed in interfaces?
      MethodList: unit }

/// <summary>
/// Defines a struct, which is a <see cref="FSharpIL.Metadata.TypeDef"/> that derives from <see cref="System.ValueType"/>.
/// </summary>
type StructDef =
   { Access: TypeAccess
     Flags: StructFlags
     StructName: NonEmptyName
     TypeNamespace: string
     FieldList: unit
     MethodList: unit }

// TODO: Should this type be a class instead to make the constructor internal?
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
                | Extends.TypeDef (Handle handle)
                | Extends.TypeRef (Handle handle) -> handle
                | Extends.Null -> ()

                match this.EnclosingClass with
                | Some(Handle parent) -> parent
                | None -> ()
            }

[<Sealed>]
type TypeDefTable internal (owner: MetadataBuilderState) =
    let defs = Table<TypeDef> owner

    member _.ToImmutable() = defs.ToImmutable()

    // TODO: Add functions for adding abstract classes and nested types.
    // TODO: Enforce CLS checks and warnings.
    member _.GetToken({ Flags = ClassFlags flags } as def: ClassDef) =
        { Flags = flags ||| def.Access.Flags
          TypeName = def.ClassName
          TypeNamespace = def.TypeNamespace
          Extends = def.Extends
          FieldList = ()
          MethodList = ()
          EnclosingClass = def.Access.EnclosingClass }
        |> defs.GetToken

    member _.GetToken({ Flags = DelegateFlags flags } as def: DelegateDef) =
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
        |> Result.bind defs.GetToken

    member _.GetToken(def: EnumDef) =
        match owner.FindType SystemType.Enum with
        | Some super ->
            { Flags = enum 0x2100 // Sealed ||| Serializable
              TypeName = def.EnumName
              TypeNamespace = def.TypeNamespace
              Extends = Extends.TypeRef super
              FieldList = ()
              MethodList = ()
              EnclosingClass = def.Access.EnclosingClass }
            |> Ok
        | None -> MissingType SystemType.Enum |> Error
        |> Result.bind defs.GetToken

    member _.GetToken({ Flags = InterfaceFlags flags } as def: InterfaceDef) =
        { Flags = flags
          TypeName = def.InterfaceName
          TypeNamespace = def.TypeNamespace
          Extends = Extends.Null
          FieldList = ()
          MethodList = ()
          EnclosingClass = def.Access.EnclosingClass }
        |> defs.GetToken

    member _.GetToken({ Flags = StructFlags flags } as def: StructDef) =
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
        |> Result.bind defs.GetToken

    interface IEnumerable<TypeDef> with
        member _.GetEnumerator() = (defs :> IEnumerable<_>).GetEnumerator()
        member _.GetEnumerator() = (defs :> System.Collections.IEnumerable).GetEnumerator()

/// II.22.15
type Field = // TODO: How to enforce that fields only have one owner?
    { Flags: unit
      Name: NonEmptyName
      Signature: unit }

    // NOTE: Equality is based on name, signature, and the owning type.
    // NOTE: Fields and methods may not need a corresponding Table type.

/// II.22.2
type Assembly =
    { HashAlgId: unit // II.23.1.1
      Version: Version
      Flags: unit
      PublicKey: unit option
      Name: AssemblyName
      Culture: AssemblyCulture }

/// II.22.5
[<ReferenceEquality; NoComparison>]
type AssemblyRef =
    { Version: Version
      Flags: unit
      PublicKeyOrToken: PublicKeyOrToken
      Name: AssemblyName
      Culture: AssemblyCulture
      HashValue: unit option }

    interface IHandleValue with member _.Handles = Seq.empty

[<Sealed>]
type AssemblyRefTable internal (owner: MetadataBuilderState) =
    let set = ImmutableHashSet.CreateBuilder<AssemblyRef>()
    let cls =
        { new IEqualityComparer<AssemblyRef> with
            member _.GetHashCode assemblyRef =
                hash (assemblyRef.Version, assemblyRef.PublicKeyOrToken, assemblyRef.Name, assemblyRef.Culture)
            member _.Equals(x, y) =
                x.Version = y.Version
                && x.PublicKeyOrToken = y.PublicKeyOrToken
                && x.Name = y.Name
                && x.Culture = y.Culture }
        |> HashSet

    member _.ToImmutable() = set.ToImmutable()

    member _.GetToken assemblyRef =
        set.Add assemblyRef |> ignore
        if cls.Add assemblyRef |> not then
            DuplicateAssemblyRef assemblyRef |> owner.Warnings.Add
        Handle(owner, assemblyRef)

// II.22.32
[<Struct; System.Runtime.CompilerServices.IsReadOnly>]
type NestedClass =
    { NestedClass: Handle<TypeDef>
      EnclosingClass: Handle<TypeDef> }

[<Sealed>]
type MetadataBuilderState () as this =
    let typeRef = TypeRefTable this // TODO: Remove "as this" and assign them in properties?
    let typeDef = TypeDefTable this

    let assemblyRef = AssemblyRefTable this

    member val internal Warnings: ImmutableArray<_>.Builder = ImmutableArray.CreateBuilder<ValidationWarning>()
    member val internal ClsChecks: ImmutableArray<_>.Builder = ImmutableArray.CreateBuilder<ClsCheck>()

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
    member _.TypeRef = typeRef
    /// (0x02)
    member _.TypeDef = typeDef
    // (0x04)
    // Field

    /// (0x20)
    member val Assembly = None with get, set // 0x20 // TODO: Figure out if None is a good default value.
    // AssemblyProcessor // 0x21 // Not used when writing a PE file
    // AssemblyOS // 0x22 // Not used when writing a PE file
    /// (0x23)
    member _.AssemblyRef = assemblyRef

    /// (0x29)
    member val NestedClass =
        Seq.choose
            (function
            | { TypeDef.EnclosingClass = Some parent } as tdef ->
                { NestedClass = Handle(this, tdef)
                  EnclosingClass = parent }
                |> Some
            | _ -> None)
            typeDef

    member internal _.FindType t =
        // TODO: Search in the TypeDefTable as well.
        typeRef.FindType t

    member internal this.EnsureOwner(value: IHandleValue) =
        for handle in value.Handles do
            if handle.Owner <> this then
                sprintf
                    "A handle to a %s owned by another state was incorrectly referenced by an %s."
                    handle.ValueType.Name
                    (value.GetType().Name)
                |> invalidArg "item"

[<Sealed>]
type MetadataTables internal (state: MetadataBuilderState) =
    /// A collection of warnings produced while creating the metadata.
    member val Warnings = state.Warnings.ToImmutable()
    member val ClsChecks = state.ClsChecks.ToImmutable()

    member val MajorVersion = state.MajorVersion
    member val MinorVersion = state.MinorVersion
    member val Module = state.Module
    member val TypeRef = state.TypeRef.ToImmutable() :> IImmutableSet<_> // TODO: Maybe use dictionaries for the TypeRef and TypeDef tables that map the type to an index?
    member val TypeDef = state.TypeDef.ToImmutable() :> IImmutableSet<_>

    member val Assembly = state.Assembly
    member val AssemblyRef = state.AssemblyRef.ToImmutable() :> IImmutableSet<_>

    // member val NestedClass = state.NestedClass.

    /// Gets a bit vector that indicates which tables are present.
    member val Valid: uint64 =
        // TODO: Update this based on the tables.
        /// NOTE: Bit zero appears to be the right-most bit.
        0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000001UL

    static member val Default = MetadataBuilderState() |> MetadataTables

[<Sealed>]
type MetadataBuilder internal () =
    member inline _.Combine(one: MetadataBuilderState -> Result<_, _>, two: MetadataBuilderState -> Result<_, ValidationError>) =
        fun state ->
            match one state with
            | Ok _ -> two state
            | Error err -> Error err
    member inline _.Bind(expr: MetadataBuilderState -> 'T, body: 'T -> MetadataBuilderState -> Result<_, ValidationError>) =
        fun state ->
            let result = expr state
            body result state
    member inline _.Bind(expr: MetadataBuilderState -> Result<'T, ValidationError>, body: 'T -> MetadataBuilderState -> _) =
        fun state ->
            match expr state with
            | Ok result -> body result state
            | Error err -> Error err
    member inline _.Delay(f: unit -> MetadataBuilderState -> Result<_, ValidationError>) = fun state -> f () state
    member inline _.For(items: seq<'T>, body: 'T -> MetadataBuilderState -> _) =
        fun state ->
            for item in items do
                body item state |> ignore
    member _.Run(expr: MetadataBuilderState -> _): ValidationResult<MetadataTables> =
        let state = MetadataBuilderState()
        match expr state with
        | Ok _ ->
            let tables = MetadataTables state
            if state.Warnings.Count > 0 then
                ValidationWarning(tables, tables.ClsChecks, tables.Warnings)
            else
                ValidationSuccess(tables, tables.ClsChecks)
        | Error err -> ValidationError err
    member inline _.Yield(expr: MetadataBuilderState -> Handle<_>) = fun state -> expr state |> Result<_, ValidationError>.Ok
    member inline _.Yield(expr: MetadataBuilderState -> Result<_, ValidationError>) = expr
    member inline _.Zero() = fun _ -> Result<_, ValidationError>.Ok()

/// <summary>
/// Contains functions for use within the <see cref="FSharpIL.Metadata.MetadataBuilder"/> computation expression.
/// </summary>
[<AutoOpen>]
module MetadataBuilder =
    /// Sets the module information of the metadata.
    let inline mdle (mdle: ModuleTable) (state: MetadataBuilderState) = state.Module <- mdle
    /// Sets the assembly information of the metadata, which specifies the version, name, and other information concerning the .NET assembly.
    let inline assembly (assembly: Assembly) (state: MetadataBuilderState) = state.Assembly <- Some assembly
    /// Adds a reference to an assembly.
    let inline assemblyRef (ref: AssemblyRef) (state: MetadataBuilderState) = state.AssemblyRef.GetToken ref
    let inline classDef (def: ClassDef) (state: MetadataBuilderState) = state.TypeDef.GetToken def
    let inline delegateDef (def: DelegateDef) (state: MetadataBuilderState) = state.TypeDef.GetToken def
    let inline enumDef (def: EnumDef) (state: MetadataBuilderState) = state.TypeDef.GetToken def
    let inline interfaceDef (def: InterfaceDef) (state: MetadataBuilderState) = state.TypeDef.GetToken def
    /// <summary>Defines a value type, which inherits from <see cref="System.ValueType"/>.</summary>
    let inline structDef (def: StructDef) (state: MetadataBuilderState) = state.TypeDef.GetToken def
    let inline typeRef (ref: TypeRef) (state: MetadataBuilderState) = state.TypeRef.GetToken ref
