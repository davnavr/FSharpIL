namespace rec FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable

/// <summary>
/// Represents a violation of a Common Language Specification rule (I.7).
/// </summary>
type ClsCheck =
    | PointerTypeUsage // of

type ValidationWarning =
    | TypeRefUsesModuleResolutionScope of TypeRef

type ValidationError =
    | DuplicateValue of IHandleValue

type ValidationResult<'Result> = ValidationResult<'Result, ClsCheck, ValidationWarning, ValidationError>

type IHandle =
    abstract Owner : MetadataBuilderState
    abstract ValueType : Type

type IHandleValue =
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

type HandleSet<'Value when 'Value :> IHandleValue and 'Value : equality> internal (owner: MetadataBuilderState, comparer: IEqualityComparer<'Value>) =
    let set = ImmutableHashSet.CreateBuilder<'Value> comparer

    new(owner: MetadataBuilderState) = HandleSet(owner, EqualityComparer.Default)

    member _.ToImmutable() = set.ToImmutable()

    abstract member GetToken : 'Value -> Result<Handle<'Value>, ValidationError>
    default _.GetToken(item: 'Value) =
        let vname = typeof<'Value>.Name
        for handle in item.Handles do
            if handle.Owner <> owner then
                sprintf
                    "A handle to a %s owned by another state was incorrectly referenced by an %s."
                    handle.ValueType.Name
                    vname
                |> invalidArg "item"
        if set.Add item |> not
        then item :> IHandleValue |> DuplicateValue |> Error
        else Handle (owner, item) |> Ok

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

    interface IHandleValue with
        member this.Handles =
            match this.ResolutionScope with
            | ResolutionScope.AssemblyRef t -> t :> IHandle |> Seq.singleton
            | _ -> Seq.empty

[<Sealed>]
type TypeRefTable internal (owner: MetadataBuilderState) =
    inherit HandleSet<TypeRef>(owner)

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

    interface IHandleValue with
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

    interface IHandleValue with member _.Handles = Seq.empty

[<Sealed>]
type MetadataBuilderState internal () as this =
    let typeRef = TypeRefTable this
    let typeDef = HandleSet<TypeDef> this

    let assemblyRef = HandleSet<AssemblyRef> this

    member val Warnings: ImmutableArray<ValidationWarning>.Builder = ImmutableArray.CreateBuilder<ValidationWarning>()
    member val ClsChecks = ImmutableArray.CreateBuilder<ClsCheck>()

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
type MetadataTables internal (state: MetadataBuilderState) =
    member val internal Warnings = state.Warnings.ToImmutable()
    member val internal ClsChecks = state.ClsChecks.ToImmutable()

    member val MajorVersion = state.MajorVersion
    member val MinorVersion = state.MinorVersion
    member val Module = state.Module
    member val TypeRef = state.TypeRef.ToImmutable() :> IImmutableSet<_>
    member val TypeDef = state.TypeDef.ToImmutable() :> IImmutableSet<_>

    member val AssemblyRef = state.AssemblyRef.ToImmutable() :> IImmutableSet<_>

    /// Gets a bit vector that indicates which tables are present.
    member val Valid: uint64 =
        // TODO: Update this based on the tables.
        /// NOTE: Bit zero appears to be the right-most bit.
        0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000001UL

    static member val Default = MetadataBuilderState() |> MetadataTables

[<Sealed>]
type MetadataBuilder internal () =
    member inline _.Combine(one: MetadataBuilderState -> _, two: MetadataBuilderState -> _) =
        fun state -> one state |> ignore; two state |> ignore;
    member inline _.Delay(f: unit -> MetadataBuilderState -> unit) = fun state -> f () state
    member inline _.For(items: seq<'T>, body: 'T -> MetadataBuilderState -> unit) =
        fun state -> for item in items do body item state
    member _.Run(expr: MetadataBuilderState -> unit): ValidationResult<MetadataTables> =
        let state = MetadataBuilderState()
        expr state
        let tables = MetadataTables state
        if state.Warnings.Count > 0 then
            ValidationWarning(tables, tables.ClsChecks, tables.Warnings)
        else
            ValidationSuccess(tables, tables.ClsChecks)
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
        state.AssemblyRef.GetToken ref
    let addTypeDef (typeDef: TypeDef) (state: MetadataBuilderState) =
        state.TypeDef.GetToken typeDef
    let addTypeRef (typeRef: TypeRef) (state: MetadataBuilderState) =
        state.TypeRef.GetToken typeRef
