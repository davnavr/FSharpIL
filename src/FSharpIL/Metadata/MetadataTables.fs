namespace rec FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable

[<StructuralEquality; NoComparison>]
type Token<'Value when 'Value : equality> =
    internal
    | Token of MetadataBuilderState * 'Value

type internal TokenDictionary<'Value when 'Value : equality> (owner: MetadataBuilderState) =
    let dict = ImmutableDictionary.CreateBuilder<Token<'Value>, 'Value>()

    interface IDictionary<Token<'Value>, 'Value> with
        member _.Add(token, value) =
            dict.Add(token, value)
        member this.Add(item: KeyValuePair<_, _>) = dict.Add item
        member _.Clear() = dict.Clear()
        member _.Contains(item: KeyValuePair<_, _>) = dict.Contains item
        member _.ContainsKey token = dict.ContainsKey token
        member _.CopyTo(array, arrayIndex) = (dict :> ICollection<_>).CopyTo(array, arrayIndex)
        member _.Count = dict.Count
        member _.GetEnumerator() = dict.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = dict.GetEnumerator() :> System.Collections.IEnumerator
        member _.IsReadOnly = (dict :> ICollection<_>).IsReadOnly
        member _.Item
            with get token = dict.Item token
            and set token item = dict.Item <- token, item
        member _.Keys = (dict :> IDictionary<_, _>).Keys
        member _.Remove(token: Token<_>) = dict.Remove token
        member _.Remove(item: KeyValuePair<_, _>) = dict.Remove item
        member _.TryGetValue(key, value) = dict.TryGetValue(key, ref value)
        member _.Values = (dict :> IDictionary<_, _>).Values

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

// II.22.38
[<StructuralComparison; StructuralEquality>]
type TypeRef =
    { ResolutionScope: unit
      TypeName: string
      TypeNamespace: string }

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

[<Sealed>]
type MetadataBuilderState () as this =
    let typeRef = TokenDictionary<TypeRef> this

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
    member val TypeRef = typeRef :> IDictionary<_, _> // TODO: Are run time recursion/initialization checks generated for this?
    /// (0x20)
    member val Assembly = None with get, set // 0x20 // TODO: Figure out if None is a good default value.
    // AssemblyProcessor // 0x21 // Not used when writing a PE file
    // AssemblyOS // 0x22 // Not used when writing a PE file
    member val AssemblyRef = ImmutableHashSet.CreateBuilder<AssemblyRef>()

[<Sealed>]
type MetadataTables (state: MetadataBuilderState) =
    member val MajorVersion = state.MajorVersion
    member val MinorVersion = state.MinorVersion
    member val Module = state.Module

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
    /// Modifies the assembly reference table.
    let inline assemblyRef (f: ISet<AssemblyRef> -> unit) (state: MetadataBuilderState) =
        f state.AssemblyRef
    /// Adds a reference to an assembly.
    let inline addAssemblyRef (ref: AssemblyRef) (state: MetadataBuilderState) =
        state.AssemblyRef.Add ref |> ignore
    let inline addTypeRef (typeRef: TypeRef) (state: MetadataBuilderState) =
        state.TypeRef.Add(Token(state, typeRef), typeRef)
