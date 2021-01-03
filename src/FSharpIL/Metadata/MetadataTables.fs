namespace rec FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable

type ValidationWarning =
    | TypeRefUsesModuleResolutionScope of TypeRef

type IHandle =
    abstract Owner : MetadataBuilderState
    abstract ValueType : Type

type IHandleIndexer =
    abstract Handles: seq<IHandle>

/// <summary>
/// Guarantees that values originate from the same <see cref="FSharpIL.Metadata.MetadataBuilderState"/>.
/// </summary>
[<NoComparison; StructuralEquality>]
type Handle<'Value when 'Value : equality> =
    private
    | Handle of MetadataBuilderState * 'Value

    member this.Item = let (Handle (_, item)) = this in item

    interface IHandle with
        member this.Owner = let (Handle (owner, _)) = this in owner
        member this.ValueType = this.Item.GetType()

type HandleSet<'Value when 'Value :> IHandleIndexer and 'Value : equality> internal (owner: MetadataBuilderState, comparer: IEqualityComparer<'Value>) =
    let set = ImmutableHashSet.CreateBuilder<'Value> comparer

    new(owner: MetadataBuilderState) = HandleSet(owner, EqualityComparer.Default)

    member _.ToImmutable() = set.ToImmutable()

    // Not having a Remove method means we won't have to check if the item of a handle is valid.

    abstract member Add : 'Value -> Handle<'Value>
    default _.Add(item: 'Value) =
        let vname = typeof<'Value>.Name
        for handle in item.Handles do
            if handle.Owner <> owner then
                sprintf
                    "A handle to a %s owned by another state was incorrectly referenced by an %s."
                    handle.ValueType.Name
                    vname
                |> invalidArg "item"
        if set.Add item |> not then
            sprintf
                "A duplicate %s was added to the set."
                vname
            |> invalidArg "item"
        Handle (owner, item)

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
    | Module // of ?? // NOTE: Does not occur in a CLI module?
    | ModuleRef // of ?
    | AssemblyRef of Handle<AssemblyRef>
    | TypeRef // of ?
    | Null

/// II.22.38
[<NoComparison; CustomEquality>]
type TypeRef =
    { ResolutionScope: ResolutionScope
      TypeName: string
      TypeNamespace: string }

    interface IEquatable<TypeRef> with
        member this.Equals other =
            this.ResolutionScope = other.ResolutionScope
            && this.TypeName = other.TypeName
            && this.TypeNamespace = other.TypeNamespace

    interface IHandleIndexer with
        member this.Handles =
            match this.ResolutionScope with
            | ResolutionScope.AssemblyRef t -> t :> IHandle |> Seq.singleton
            | _ -> Seq.empty

[<Sealed>]
type TypeRefTable internal (owner: MetadataBuilderState) =
    let typeSet = HandleSet<TypeRef> owner
    let clsSet =
        ImmutableHashSet.CreateBuilder<TypeRef>

    member _.ToImmutable() = typeSet.ToImmutable()

    member _.Add typeRef =
        let token = typeSet.Add typeRef
        let cls =
            [
                // TODO: Check that the name is a "valid CLS identifier".
            ]
        match typeRef.ResolutionScope with
        | ResolutionScope.Module -> ValidationWarning(token, cls, [ TypeRefUsesModuleResolutionScope typeRef ])
        | _ -> ValidationSuccess(token, cls)

/// <summary>
/// Specifies which type a <see cref="FSharpIL.Metadata.TypeDef"/> extends.
/// </summary>
[<NoComparison; StructuralEquality>]
[<RequireQualifiedAccess>]
type Extends =
    | TypeDef of Handle<TypeDef>
    /// <summary>
    /// Indicates that a class does not extend another class, used for <see cref="System.Object"/>.
    /// </summary>
    | Null

// TODO: Enforce CLS checks by inheriting from HandleSet.
// TODO: Maybe make this a union, and define cases for classes, interfaces, and structs.
/// II.22.37
[<CustomEquality; NoComparison>]
type TypeDef =
    { Flags: unit
      TypeName: string
      TypeNamespace: string
      Extends: Extends
      FieldList: unit
      MethodList: unit }

    interface IEquatable<TypeDef> with
        member this.Equals other =
            this.Extends = other.Extends
            && this.TypeNamespace = other.TypeNamespace
            && this.TypeName = other.TypeName

    interface IHandleIndexer with
        member this.Handles =
            seq {
                match this.Extends with
                | Extends.TypeDef t -> t :> IHandle
                | Extends.Null -> ()
            }

/// II.22.15
type Field = // TODO: How to enforce that fields only have one owner?
    { Flags: unit
      Name: NonEmptyName
      Signature: unit }

    // NOTE: Equality is based on name, signature, and the owning type.

/// II.22.2
type AssemblyTable =
    { HashAlgId: unit // II.23.1.1
      Version: Version
      Flags: unit
      PublicKey: unit option
      Name: AssemblyName
      Culture: AssemblyCulture }

    static member Default =
        { HashAlgId = ()
          Version = Version(1, 0, 0, 0)
          Flags = ()
          PublicKey = None
          Name = AssemblyName "Default"
          Culture = NullCulture }

/// II.22.5
[<CustomEquality; NoComparison>]
type AssemblyRef =
    { Version: Version
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

    interface IHandleIndexer with member _.Handles = Seq.empty

[<Sealed>]
type MetadataBuilderState () as this =
    let typeRef = TypeRefTable this
    let typeDef = HandleSet<TypeDef> this

    let assemblyRef = HandleSet<AssemblyRef> this

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
    /// (0x04)


    /// (0x20)
    member val Assembly = None with get, set // 0x20 // TODO: Figure out if None is a good default value.
    // AssemblyProcessor // 0x21 // Not used when writing a PE file
    // AssemblyOS // 0x22 // Not used when writing a PE file
    member _.AssemblyRef = assemblyRef

[<Sealed>]
type MetadataTables (state: MetadataBuilderState) =
    member val MajorVersion = state.MajorVersion
    member val MinorVersion = state.MinorVersion
    member val Module = state.Module
    member val TypeRef = state.TypeRef.ToImmutable()
    member val TypeDef = state.TypeDef.ToImmutable()

    member val AssemblyRef = state.AssemblyRef.ToImmutable()

    /// Gets a bit vector that indicates which tables are present.
    member val Valid: uint64 =
        // TODO: Update this based on the tables.
        /// NOTE: Bit zero appears to be the right-most bit.
        0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000001UL

    static member val Default = MetadataBuilderState() |> MetadataTables

[<Sealed>]
type MetadataBuilder internal () =
    member inline _.Combine(one: MetadataBuilderState -> unit, two: MetadataBuilderState -> unit) =
        fun state -> one state; two state;
    member inline _.Delay(f: unit -> MetadataBuilderState -> unit) = fun state -> f () state
    member inline _.For(items: seq<'T>, body: 'T -> MetadataBuilderState -> unit) =
        fun state -> for item in items do body item state
    member inline _.Run(expr: MetadataBuilderState -> unit) =
        let state = MetadataBuilderState()
        expr state
        MetadataTables state
    member inline _.Yield(expr: MetadataBuilderState -> _) = expr >> ignore
    member inline _.Zero() = ignore<MetadataBuilderState>

[<AutoOpen>]
module MetadataBuilder =
    /// Sets the module information of the metadata.
    let inline mdle (mdle: ModuleTable) (state: MetadataBuilderState) =
        state.Module <- mdle
    /// Sets the assembly information of the metadata, which specifies the version, name, and other information concerning the .NET assembly.
    let inline assembly (assembly: AssemblyTable) (state: MetadataBuilderState) =
        state.Assembly <- Some assembly
    /// Adds a reference to an assembly.
    let addAssemblyRef (ref: AssemblyRef) (state: MetadataBuilderState) =
        state.AssemblyRef.Add ref
    let addTypeDef (typeDef: TypeDef) (state: MetadataBuilderState) =
        state.TypeDef.Add typeDef
    let addTypeRef (typeRef: TypeRef) (state: MetadataBuilderState) =
        state.TypeRef.Add typeRef
