namespace rec FSharpIL.Metadata

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Runtime.CompilerServices

open FSharpIL.Metadata

// TODO: Consider moving some table types and the MetadataBuilder class to a file below this one. Maybe move each table and its related types to a separate file?

/// <summary>
/// Represents a violation of a Common Language Specification rule (I.7).
/// </summary>
[<Obsolete>]
type ClsViolation =
    /// A violation of rule 19, which states that "CLS-compliant interfaces shall not define...fields".
    | InterfaceContainsFields of InterfaceDef

// TODO: Replace error DU with interface, or maybe use exceptions?
[<Obsolete>]
type ValidationError =
    // TODO: Have different cases for different duplicate values.
    // TODO: Consider replacing errors that occur for duplicate values with something else.
    /// Error used when a duplicate object was added to a table, or when a duplicate method or field is added to a type.
    | DuplicateValue of obj
    | DuplicateFile of File
    | MissingType of ns: string * Identifier

    override this.ToString() =
        match this with
        | DuplicateValue value -> value.GetType().Name |> sprintf "Cannot add duplicate %s"
        | DuplicateFile file -> sprintf "A file with the name %A already exists in the file table" file.FileName
        | MissingType(ns, name) ->
            match ns with
            | "" -> string name
            | _ -> sprintf "%s.%A" ns name
            |> sprintf "Unable to find type \"%s\", perhaps a TypeDef or TypeRef is missing"









[<RequireQualifiedAccess>]
type MemberRefParent =
    // | MethodDef // of ?
    // | ModuleRef // of ?
    // | TypeDef // of ?
    | TypeRef of SimpleIndex<TypeRef>
    // | TypeSpec // of ?

    interface IIndexValue with
        member this.CheckOwner actual =
            match this with
            | TypeRef (IndexOwner owner) ->
                actual.EnsureEqual owner

type MemberRef<'Signature> =
    { Class: MemberRefParent
      MemberName: Identifier
      Signature: 'Signature }

type MethodRef = MemberRef<MethodRefSignature>

// type FieldRef = 

/// <summary>
/// Represents a row in the <c>MemberRef</c> table, which contains references to the methods and fields of a class (II.22.25).
/// </summary>
/// <seealso cref="T:FSharpIL.Metadata.MethodRef"/>
/// <seealso cref="T:FSharpIL.Metadata.FieldRef"/>
[<StructuralComparison; StructuralEquality>]
type MemberRefRow =
    | MethodRef of MethodRef
    // | FieldRef // of ?

    member this.Class =
        match this with
        | MethodRef { Class = parent } -> parent

    member this.MemberName =
        match this with
        | MethodRef { MemberName = name } -> name

    interface IIndexValue with
        member this.CheckOwner actual =
            match this with
            | MethodRef method ->
                actual.CheckOwner method.Class
                () // TODO: Check method ref signature for valid index owner.

type MemberRefIndex<'Member> = TaggedIndex<'Member, MemberRefRow>

[<Sealed>]
type MemberRefTable internal (owner: IndexOwner) =
    let members = MutableTable<MemberRefRow> owner

    member _.Count = members.Count

    // TODO: Enforce CLS checks.
    // NOTE: Duplicates (based on owning class, name, and signature) are allowed, but produce a warning.
    member private _.GetIndex<'MemberRef>(row: MemberRefRow) =
        members.GetIndex row
        |> Option.map MemberRefIndex
        |> Option.defaultWith (fun() -> MemberRefIndex<'MemberRef>(owner, row))

    member this.GetIndex(method: MethodRef) = this.GetIndex<MethodRef>(MethodRef method)
    // member this.GetIndex(field: FieldRef) = this.GetIndex<FieldRef>(FieldRef field)

    interface IReadOnlyCollection<MemberRefRow> with
        member _.Count = members.Count
        member _.GetEnumerator() = members.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = members.GetEnumerator() :> System.Collections.IEnumerator




[<RequireQualifiedAccess>]
type CustomAttributeParent =
    // | MethodDef // of ?
    // | Field // of ?
    // | TypeRef // of ?
    | TypeDef of SimpleIndex<TypeDefRow>
    // | Param // of ?
    // | InterfaceImpl // of ?
    // | MemberRef of SimpleIndex<MemberRefRow>
    // | Module // of ?
    // | Permission // of ?
    // | Property // of ?
    // | Event // of ?
    // | StandAloneSig // of ?
    // | ModuleRef // of ?
    // | TypeSpec // of ?
    | Assembly of AssemblyIndex
    // | AssemblyRef // of ?
    // | File // of ?
    // | ExportedType // of ?
    // | ManifestResource // of ?
    // | GenericParam // of ?
    // | GenericParamConstraint // of ?
    // | MethodSpec // of ?

[<RequireQualifiedAccess>]
type CustomAttributeType =
    // | MethodDef // of ?
    | MemberRef of MemberRefIndex<MethodRef>

/// <summary>Represents a row in the <c>CustomAttribute</c> table (II.22.10).</summary>
type CustomAttribute =
    { Parent: CustomAttributeParent
      /// Specifies the constructor method used to create the custom attribute.
      Type: CustomAttributeType // TODO: How to ensure that the MethodRef points to a .ctor?
      Value: CustomAttributeSignature option }
      // TODO: How to validate signature to ensure types of fixed arguments match method signature? Maybe have FixedArgs field of signature type be ParamItem -> int -> FixedArg?

    interface IIndexValue with
        member this.CheckOwner actual =
            match this.Parent with
            | CustomAttributeParent.TypeDef (IndexOwner owner)
            | CustomAttributeParent.Assembly (IndexOwner owner) ->
                actual.EnsureEqual owner

            match this.Type with
            | CustomAttributeType.MemberRef (IndexOwner owner) ->
                actual.EnsureEqual owner

            () // TODO: Ensure signature references valid things with the same owner.

[<Sealed>]
type CustomAttributeTable internal (owner: IndexOwner) =
    let attrs = List<CustomAttribute>()

    member _.Count = attrs.Count

    member _.Add(attr: CustomAttribute) =
        owner.CheckOwner attr
        attrs.Add attr

    interface IReadOnlyCollection<CustomAttribute> with
        member _.Count = attrs.Count
        member _.GetEnumerator() = attrs.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = attrs.GetEnumerator() :> System.Collections.IEnumerator





// II.22.32
[<Struct; IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type NestedClass =
    { NestedClass: SimpleIndex<TypeDefRow>
      EnclosingClass: SimpleIndex<TypeDefRow> }

    interface IIndexValue with
        member this.CheckOwner actual =
            actual.EnsureEqual this.NestedClass.Owner
            actual.EnsureEqual this.EnclosingClass.Owner







/// <summary>Represents a <c>MethodRefSig</c>, which "provides the call site Signature for a method" (II.23.2.2).</summary>
[<IsReadOnly; Struct>]
type MethodRefSignature =
    { HasThis: bool
      ExplicitThis: bool
      /// <summary>Corresponds to the <c>RetType</c> item, which specifies the return type of the method.</summary>
      ReturnType: ReturnTypeItem
      Parameters: ImmutableArray<ParamItem>
      VarArgParameters: ImmutableArray<ParamItem> }

    member this.ParamCount = uint32 (this.Parameters.Length + this.VarArgParameters.Length)

    member internal this.CallingConventions =
        let mutable flags = CallingConvention.Default
        if this.HasThis then flags <- flags ||| CallingConvention.HasThis
        if this.ExplicitThis then flags <- flags ||| CallingConvention.ExplicitThis
        if not this.VarArgParameters.IsEmpty then flags <- flags ||| CallingConvention.VarArg
        flags

/// Captures the definition of a field or global variable (II.23.2.4).
[<IsReadOnly; Struct>]
type FieldSignature =
    { CustomMod: ImmutableArray<CustomModifier>
      FieldType: ReturnTypeItem }





/// II.23.2.13
[<IsReadOnly; Struct>]
type ArrayShape =
    { /// Specifies the number of dimensions in the array.
      Rank: uint32 // TODO: How to prevent a value of zero?
      /// <summary>Specifies the sizes of each dimension.</summary>
      /// <remarks>Corresponds to the <c>NumSizes</c> item and <c>Size</c> items in the signature.</remarks>
      Sizes: ImmutableArray<uint32> // NOTE: The two arrays containing information for each dimension can contain less items than Rank.
      /// <summary>Specifies the lower bounds of each dimension.</summary>
      /// <remarks>Corresponds to the <c>NumLoBounds</c> item and <c>LoBound</c> items in the signature.</remarks>
      LowerBounds: ImmutableArray<int32> }

    /// Describes the shape of a single-dimensional array.
    static member OneDimension = { Rank = 1u; Sizes = ImmutableArray.Empty; LowerBounds = ImmutableArray.Empty }

/// <summary>Represents a <c>FixedArg</c> item, which stores the arguments for a custom attribute's constructor method (II.23.3).</summary>
[<RequireQualifiedAccess>]
type FixedArg =
    | Elem of Elem
    | SZArray of ImmutableArray<Elem>

[<RequireQualifiedAccess>]
type NamedArg =
    | Field // of FieldOrPropType * string * FixedArg
    | Property // of FieldOrPropType * string * FixedArg

/// <summary>Represents an <c>Elem</c> item, which is an argument in a custom attribute (II.23.3).</summary>
type Elem =
    | ValBool of bool
    | ValChar of char
    | ValR4 of float32
    | ValR8 of System.Double
    | ValI1 of int8
    | ValU1 of uint8
    | ValI2 of int16
    | ValU2 of uint16
    | ValI4 of int32
    | ValU4 of uint32
    | ValI8 of int64
    | ValU8 of uint64
    // | ValEnum // of SomehowGetTheEnumUnderlyingType?
    /// <summary>Represents a string used as an argument in a custom attribute.</summary>
    /// <remarks>Empty strings and <see langword="null"/> strings are allowed values.</remarks>
    | SerString of string
    // | SerStringType // of SomehowGetTheCanonicalNameOfType.
    // | BoxedObject of // underlying value.

/// <summary>
/// Represents a <c>CustomAttrib</c>, which stores the arguments provided to a custom attribute's constructor,
/// as well as any values assigned to its fields or properties. (II.23.3).
/// </summary>
type CustomAttributeSignature =
    { FixedArg: ImmutableArray<FixedArg>
      NamedArg: ImmutableArray<NamedArg> }




/// <summary>(0x00) Represents the single row of the <c>Module</c> table (II.22.30).</summary>
type ModuleTable =
    { // Generation
      Name: Identifier
      Mvid: Guid
      // EncId
      // EncBaseId
      }

[<Sealed>]
type MetadataBuilderState (mdle: ModuleTable) =
    let owner = IndexOwner()
    let warnings = ImmutableArray.CreateBuilder<ValidationWarning>()
    let clsViolations = ImmutableArray.CreateBuilder<ClsViolation>()
    let mutable entrypoint = None

    let typeDef = TypeDefTable owner
    let mutable assembly = None

    member internal _.Owner = owner

    member val Header = CliHeaderFields.Default with get, set

    member this.HeaderFlags =
        let signed =
            if this.Header.StrongNameSignature.IsEmpty
            then CorFlags.None
            else CorFlags.StrongNameSigned
        CorFlags.ILOnly ||| signed

    /// The metadata version, contained in the metadata root (II.24.2.1).
    member val MetadataVersion = MetadataVersion.ofStr "v4.0.30319" with get, set

    member val Warnings = warnings
    member val ClsViolations = clsViolations

    // Reserved: uint32
    member val MajorVersion: byte = 2uy
    member val MinorVersion: byte = 0uy
    // HeapSizes: byte
    // Reserved: byte
    // Valid: uint64
    // Sorted: uint64 // TODO: Figure out what Sorted is used for.
    // Rows
    /// (0x00)
    member val Module = mdle
    /// (0x01)
    member val TypeRef: TypeRefTable = TypeRefTable(owner, warnings)
    /// (0x02)
    member _.TypeDef: TypeDefTable = typeDef
    // (0x04)
    // member Field
    // (0x06)
    // member Method
    // (0x08)
    // member Param
    // (0x09)
    // member InterfaceImpl
    /// (0x0A)
    member val MemberRef: MemberRefTable = MemberRefTable owner
    // (0x0B)
    // member Constant
    /// (0x0C)
    member val CustomAttribute: CustomAttributeTable = CustomAttributeTable owner
    // (0x0D)
    // member FieldMarshal
    // (0x0E)
    // member DeclSecurity
    // (0x0F)
    // member ClassLayout
    // (0x10)
    // member FieldLayout
    // (0x11)
    // member StandAloneSig
    // (0x12)
    // member EventMap
    // (0x14)
    // member Event
    // (0x15)
    // member PropertyMap
    // (0x17)
    // member Property
    // (0x18)
    // member MethodSemantics
    // (0x19)
    // member MethodImpl
    /// (0x1A)
    member val ModuleRef = ModuleRefTable owner
    // (0x1B)
    // member TypeSpec
    // (0x1C)
    // member ImplMap
    // (0x1D)
    // member FieldRva
    /// (0x20)
    member _.Assembly: Assembly option = assembly
    // AssemblyProcessor // 0x21 // Not used when writing a PE file
    // AssemblyOS // 0x22 // Not used when writing a PE file
    /// (0x23)
    member val AssemblyRef: AssemblyRefTable = AssemblyRefTable owner
    // AssemblyRefProcessor // 0x24 // Not used when writing a PE file
    // AssemblyRefOS // 0x25 // Not used when writing a PE file
    /// (0x26)
    member val File = FileTable owner
    // (0x27)
    // member ExportedType
    // (0x28)
    // member ManifestResource
    /// (0x29)
    member val NestedClass =
        Seq.choose
            (fun (tdef: TypeDefRow) ->
                match tdef.EnclosingClass with
                | Some parent ->
                    { NestedClass = SimpleIndex(owner, tdef)
                      EnclosingClass = parent }
                    |> Some
                | _ -> None)
            typeDef
    // (0x2A)
    // member GenericParam
    // (0x2B)
    // member MethodSpec
    // (0x2C)
    // member GenericParamConstraint

    // TODO: How to specify the entrypoint in a multi-file assembly? Create an EntryPoint DU.
    /// <summary>Gets or sets the entrypoint of the assembly.</summary>
    /// <remarks>The entrypoint of the assembly is specified by the <c>EntryPointToken</c> field of the CLI header (II.25.3.3).</remarks>
    member _.EntryPoint
        with get(): SimpleIndex<MethodDef> option = entrypoint
        and set main =
            match main with
            | Some (main': SimpleIndex<_>) ->
                if main'.Owner <> owner then
                    invalidArg "main" "The specified entrypoint cannot be owned by another state."
                IndexOwner.checkIndex owner main'
                entrypoint <- Some main'
            | None -> entrypoint <- None

    member _.SetAssembly(assm: Assembly) =
        assembly <- Some assm
        AssemblyIndex(owner, ())

    member internal this.FindType t: SimpleIndex<_> option =
        // TODO: Search in the TypeDefTable as well.
        this.TypeRef.FindType t

    member internal _.CreateTable table = ImmutableTable(table, fun item -> SimpleIndex(owner, item))
