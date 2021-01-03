﻿namespace rec FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable

// TODO: Have a Result type that allows Success, Warning, or Error.

type internal IHandle =
    abstract Owner : MetadataBuilderState

type internal IHandleIndexer =
    abstract Handles: seq<IHandle>

/// <summary>
/// Guarantees that values originate from the same <see cref="FSharpIL.Metadata.MetadataBuilderState"/>.
/// </summary>
[<NoComparison; StructuralEquality>]
type Handle<'Value when 'Value : equality> =
    internal
    | Token of MetadataBuilderState * 'Value

    member this.Item = let (Token (_, item)) = this in item

    interface IHandle with
        member this.Owner = let (Token (owner, _)) = this in owner

type HandleSet<'Value when 'Value :> IHandleIndexer and 'Value : equality> internal (owner: MetadataBuilderState) = // TODO: Have a mechanism to check that any tokens 'Value has have the same owner.
    let set = ImmutableHashSet.CreateBuilder<'Value> ()

    member internal _.ToImmutable() = set.ToImmutable()
    member internal _.Add(item: 'Value) =
        for handle in item.Handles do
            if handle.Owner <> owner then
                invalidOp "A handle owned by another state was incorrectly referenced."
        set.Add item

// II.22.30
type ModuleTable =
    { // Generation
      Name: ModuleName
      Mvid: Guid
      // EncId
      // EncBaseId
      }

    static member Default =
        { Name = ModuleName "Default.dll"
          Mvid = Guid.Empty } // TODO: What should the default Mvid be?

[<NoComparison; StructuralEquality>]
[<RequireQualifiedAccess>]
type ResolutionScope =
    | Module // of ?? // NOTE: Does not occur in a CLI module?
    | ModuleRef // of ?
    | AssemblyRef of Handle<AssemblyRef>
    | TypeRef // of ?
    | Null

// II.22.38
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

// II.22.37
[<CustomEquality; NoComparison>]
type TypeDef = // TODO: Maybe make this a union, and define cases for classes, interfaces, and structs.
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

// II.22.2
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

// II.22.5
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
    let typeRef = HandleSet<TypeRef> this
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
        /// NOTE: Bit zero appears to be the right-most bit.
        0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_00000001UL

[<Sealed>]
type MetadataBuilder internal () =
    member inline _.Combine(one: MetadataBuilderState -> unit, two: MetadataBuilderState -> unit) =
        fun state -> one state; two state;
    member inline _.Delay(f: unit -> MetadataBuilderState -> unit) = fun state -> f () state
    member inline _.For(items: seq<'T>, body: 'T -> MetadataBuilderState -> unit) =
        fun state -> for item in items do body item state
    member inline _.Run(expr: MetadataBuilderState -> unit) = MetadataBuilderState() |> expr
    member inline _.Yield(expr: MetadataBuilderState -> unit) = expr
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
    let inline addAssemblyRef (ref: AssemblyRef) (state: MetadataBuilderState) =
        state.AssemblyRef.Add ref |> ignore
    let inline addTypeRef (typeRef: TypeRef) (state: MetadataBuilderState) =
        state.TypeRef.Add typeRef
