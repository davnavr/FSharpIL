﻿namespace FSharpIL.Metadata.Tables

open System
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.PortableExecutable

open FSharpIL.Utilities
open FSharpIL.Utilities.Compare

[<IsReadOnly; Struct>]
type ModuleRow =
    { Generation: uint16
      Name: IdentifierOffset
      Mvid: GuidIndex
      EncId: GuidIndex
      EncBaseId: GuidIndex }
    interface ITableRow

[<RequireQualifiedAccess>]
module ModuleRow =
    let inline create name mvid =
        { Generation = 0us
          Name = name
          Mvid = mvid
          EncId = Unchecked.defaultof<_>
          EncBaseId = Unchecked.defaultof<_> }

/// <summary>(0x01) Represents a row in the <c>TypeRef</c> table (II.22.38).</summary>
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type TypeRefRow =
    { ResolutionScope: ResolutionScope
      TypeName: IdentifierOffset
      TypeNamespace: StringOffset }

    interface ITableRow

/// <summary>Describes the attributes of a <c>Field</c> (II.23.1.5).</summary>
[<Flags>]
type FieldFlags =
    | FieldAccessMask = 7us
    | CompilerControlled = 0us
    /// The field is only visible to the containing type.
    | Private = 1us
    /// The field is only visible to the containing type, or types derived from the containing type in the same assembly.
    | FamAndAssem = 2us
    /// The field is only visible to types in the same assembly.
    | Assembly = 3us
    /// The field is only visible to the containing type or types derived from the containing type.
    | Family = 4us
    /// The field is only visible to the containing type, types derived from the containing type, and types in the same assembly.
    | FamOrAssem = 5us
    | Public = 6us
    | Static = 0x10us
    | InitOnly = 0x20us
    | Literal = 0x40us
    | NotSerialized = 0x80us
    | SpecialName = 0x200us
    | PInvokeImpl = 0x2000us
    | RTSpecialName = 0x400us
    | HasFieldMarshal = 0x1000us
    | HasDefault = 0x8000us
    | HasFieldRva = 0x100us

/// <summary>(0x04) Represents a row in the <c>Field</c> table (II.22.15).</summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type FieldRow =
    { Flags: FieldFlags
      Name: IdentifierOffset
      Signature: FieldSigOffset }

    member this.Equals other =
        this.Name === other.Name &&
        this.Signature === other.Signature &&
        (this.Flags &&& other.Flags &&& FieldFlags.FieldAccessMask) <> FieldFlags.CompilerControlled

    override this.GetHashCode() = HashCode.Combine(this.Name, this.Signature)

    override this.Equals obj =
        match obj with
        | :? FieldRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<FieldRow> with member this.Equals other = this.Equals(other = other)

/// <summary>Describes the attributes of a <c>MethodDef</c> (II.23.1.10).</summary>
[<Flags>]
type MethodDefFlags =
    | MemberAccessMask = 7us
    | CompilerControlled = 0us
    /// The method is only visible to the containing type.
    | Private = 1us
    /// The method is only visible to the containing type, or types derived from the containing type in the same assembly.
    | FamAndAssem = 2us
    /// The method is only visible to types in the same assembly.
    | Assembly = 3us
    /// This method is only visible to the containing type or types derived from the containing type.
    | Family = 4us
    /// The method is only visible to the containing type, types derived from the containing type, and types in the same assembly.
    | FamOrAssem = 5us
    | Public = 6us
    | Static = 0x10us
    /// The method cannot be overriden.
    | Final = 0x20us
    /// The method can be overriden.
    | Virtual = 0x40us
    | HideBySig = 0x80us
    | VTableLayoutMask = 0x100us
    | ReuseSlot = 0us
    | NewSlot = 0x100us
    /// The method can only be overriden if it is also accessible.
    | Strict = 0x200us
    /// The method does not provide an implementation.
    | Abstract = 0x400us
    | SpecialName = 0x800us
    | PInvokeImpl = 0x2000us
    | UnmanagedExport = 0x8us
    /// Used by constructors and class constructors.
    | RTSpecialName = 0x1000us
    | HasSecurity = 0x4000us
    | RequireSecObject = 0x8000us

/// <summary>Describes how a <c>MethodDef</c> is implemented (II.23.1.11).</summary>
[<Flags>]
type MethodImplFlags =
    | CodeTypeMask = 3us
    /// The method implementation is in Common Intermediate Language.
    | IL = 0us
    /// The method implementation is in native code.
    | Native = 1us
    /// <summary>
    /// The method implementation is in Optimized Intermediate Language, which is a subset of Common Intermediate Language used
    /// to help produce native code.
    /// </summary>
    | OptIL = 2us
    /// <summary>The method implementation is provided by the runtime.</summary>
    /// <remarks>This flag is used for methods defined on delegate types.</remarks>
    | Runtime = 3us
    | ManagedMask = 4us
    | Unmanaged = 4us
    | Managed = 0us
    | ForwardRef = 0x10us
    | PreserveSig = 0x80us
    | InternalCall = 0x1000us
    | Synchronized = 0x20us
    /// The method cannot be inlined.
    | NoInlining = 8us
    /// The method will not be optimized during native code generation.
    | NoOptimization = 0x40us

/// <summary>Describes the attributes of a <c>Param</c> (II.23.1.13).</summary>
[<Flags>]
type ParamFlags =
    | None = 0us
    | In = 1us
    | Out = 2us
    | Optional = 0x10us
    | HasDefault = 0x1000us
    | HasFieldMarshal = 0x2000us
    | Reserved = 0xCFE0us

/// <summary>(0x08) Represents a row in the <c>Param</c> table (II.22.33).</summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type ParamRow =
    { Flags: ParamFlags
      Sequence: uint16
      Name: StringOffset }

    member this.Equals other = this.Sequence = other.Sequence

    override this.GetHashCode() = int32 this.Sequence

    override this.Equals obj =
        match obj with
        | :? ParamRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<ParamRow> with member this.Equals other = this.Equals other




[<IsReadOnly>]
type MethodBodyLocation = struct
    val Value: uint32
    internal new (value) = { Value = value }
    member this.IsNull = this.Value = 0u
    static member (+) (location: MethodBodyLocation, start: Rva) = location.Value + start
end

/// <summary>(0x06) Represents a row in the <c>MethodDef</c> table (II.22.26).</summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type MethodDefRow =
    { Rva: MethodBodyLocation
      ImplFlags: MethodImplFlags
      Flags: MethodDefFlags
      Name: IdentifierOffset
      Signature: MethodDefSigOffset
      ParamList: TableIndex<ParamRow> }

    member this.Equals other =
        this.Name === other.Name &&
        this.Signature === other.Signature &&
        (this.Flags &&& other.Flags &&& MethodDefFlags.MemberAccessMask) <> MethodDefFlags.CompilerControlled

    override this.GetHashCode() = HashCode.Combine(this.Name, this.Signature)

    override this.Equals obj =
        match obj with
        | :? MethodDefRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<MethodDefRow> with member this.Equals other = this.Equals other

/// <summary>
/// Describes the attributes of a <c>TypeDef</c>, such as its visibility, layout, string formatting, and other information
/// (II.23.1.15).
/// </summary>
[<Flags>]
type TypeDefFlags =
    | VisibilityMask = 7u
    | NotPublic = 0u
    | Public = 1u
    | NestedPublic = 2u
    /// The type is nested and is only visible to the containing type.
    | NestedPrivate = 3u
    /// The type is nested and is only visible to the containing type and types that are derived from the containing type.
    | NestedFamily = 4u
    /// The type is nested and is only visible to types in the same assembly.
    | NestedAssembly = 5u
    /// The type is nested and is only visible to the containing type and types that are derived from the containing type in the
    /// same assembly.
    | NestedFamAndAssem = 6u
    /// The type is nested and is only visible to the containing type, types that are in the same assembly, and types that are
    /// derived from the containing type.
    | NestedFamOrAssem = 7u
    /// Used to obtain the layout information of the class.
    | LayoutMask = 0x18u
    /// The fields of the type are laid out automatically by the CLR.
    | AutoLayout = 0u
    /// <summary>
    /// The fields of the type are laid out sequentiallly according to the order of the fields in the metadata.
    /// </summary>
    /// <remarks>This is used as the default layout for structs by the C# and F# compilers.</remarks>
    | SequentialLayout = 8u
    /// The layout information for the fields of the type is explicitly provided.
    | ExplicitLayout = 0x10u
    | ClassSemanticsMask = 0x20u
    | Class = 0u
    | Interface = 0x20u
    /// The type cannot be instantiated, only instances of derived types can be created.
    | Abstract = 0x80u
    /// The type cannot be inherited from.
    | Sealed = 0x100u
    | SpecialName = 0x400u
    | Import = 0x1000u
    | Serializable = 0x2000u
    | StringFormatMask = 0x30000u
    | AnsiClass = 0u
    | UnicodeClass = 0x10000u
    | AutoClass = 0x20000u
    /// A non-standard encoding is used when marshalling strings.
    | CustomFormatClass = 0x30000u
    /// Used to obtain non-standard encoding information used when marshalling strings.
    | CustomStringFormatMask = 0xC00000u
    /// The type can be initialized before the first access to any of its static fields.
    | BeforeFieldInit = 0x100000u
    | RTSpecialName = 0x800u
    | HasSecurity = 0x40000u
    | IsTypeForwarder = 0x200000u

/// <summary>(0x02) Represents a row in the <c>TypeDef</c> table (II.22.37).</summary>
[<IsReadOnly; Struct>]
type TypeDefRow = // TODO: Figure out how to handle equality for TypeDefRow, see TypeDefTableBuilder.fs
    { Flags: TypeDefFlags
      TypeName: IdentifierOffset
      TypeNamespace: StringOffset
      Extends: TypeDefOrRef
      FieldList: TableIndex<FieldRow>
      MethodList: TableIndex<MethodDefRow> }
    interface ITableRow

    member this.IsInterface = Flags.set TypeDefFlags.Interface this.Flags

/// <summary>(0x09) Represents a row in the <c>InterfaceImpl</c> table (II.22.23).</summary>
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type InterfaceImplRow =
    { Class: TableIndex<TypeDefRow>; Interface: TypeDefOrRef }
    interface ITableRow

/// <summary>
/// (0x0A) Represents a row in the <c>MemberRef</c> table, which contains references to methods or fields (II.22.25).
/// </summary>
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type MemberRefRow =
    { Class: MemberRefParent
      Name: IdentifierOffset
      Signature: MemberRefSigOffset }
    interface ITableRow

/// Specifies the type of a constant value (II.22.9).
type ConstantType = // TODO: Make ConstantType a struct DU.
    | Boolean = 0x2uy
    | Char = 0x3uy
    | I1 = 0x4uy
    | U1 = 0x5uy
    | I2 = 0x6uy
    | U2 = 0x7uy
    | I4 = 0x8uy
    | U4 = 0x9uy
    | I8 = 0xAuy
    | U8 = 0xBuy
    | R4 = 0xCuy
    | R8 = 0xDuy
    | String = 0xEuy
    | Null = 0x12uy

[<RequireQualifiedAccess>]
module ConstantType =
    let inline toElementType (constantType: ConstantType) = LanguagePrimitives.EnumOfValue<_, ElementType>(uint8 constantType)

/// <summary>
/// (0x0B) Represents a row in the <c>Constant</c> table, which contains "compile-time, constant values for fields, parameters,
/// and properties" (II.22.9).
/// </summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type ConstantRow =
    { Type: ConstantType
      Parent: HasConstant
      Value: ConstantOffset }

    member this.Equals other = this.Parent === other.Parent

    override this.GetHashCode() = this.Parent.GetHashCode()

    override this.Equals obj =
        match obj with
        | :? ConstantRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<ConstantRow> with member this.Equals other = this.Equals(other = other)

/// <summary>(0x0C) Represents a row in the <c>CustomAttribute</c> table (II.22.10).</summary>
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type CustomAttributeRow =
    { Parent: HasCustomAttribute
      Type: CustomAttributeType
      Value: CustomAttributeOffset }
    interface ITableRow



/// <summary>
/// (0x0F) Represents a row in the <c>ClassLayout</c> table, which describes how the fields of a type are laid out by the Common
/// Language Infrastructure (II.22.8).
/// </summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type ClassLayoutRow =
    { PackingSize: uint16
      ClassSize: uint32
      Parent: TableIndex<TypeDefRow> }

    member this.Equals other = this.Parent === other.Parent

    override this.GetHashCode() = this.Parent.GetHashCode()

    override this.Equals obj =
        match obj with
        | :? ConstantRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<ClassLayoutRow> with member this.Equals other = this.Equals(other = other)



/// <summary>
/// (0x11) Represents a row in the <c>StandAloneSig</c> table, which contains an offset into the <c>#Blob</c> heap (II.22.36).
/// </summary>
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type StandaloneSigRow =
    internal { StandAloneSig: BlobOffset }
    member this.Signature = this.StandAloneSig
    interface ITableRow

/// <summary>Describes the attributes of an <c>Event</c> (II.23.1.4).</summary>
[<Flags>]
type EventFlags =
    | None = 0us
    | SpecialName = 0x200us
    | RTSpecialName = 0x400us

/// <summary>
/// (0x14) Represents a row in the <c>Event</c> table (II.22.13).
/// </summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type EventRow =
    { EventFlags: EventFlags
      Name: IdentifierOffset
      EventType: TypeDefOrRef }

    member this.Equals other = this.Name === other.Name

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals obj =
        match obj with
        | :? EventRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<EventRow> with member this.Equals other = this.Equals other

/// <summary>
/// (0x12) Represents a row in the <c>EventMap</c> table, which specifies the events associated with a type (II.22.12).
/// </summary>
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type EventMapRow =
    { Parent: TableIndex<TypeDefRow>
      EventList: TableIndex<EventRow> }
    interface ITableRow



/// <summary>Describes the attributes of a <c>Property</c> (II.23.1.14).</summary>
[<Flags>]
type PropertyFlags =
    | None = 0us
    | SpecialName = 0x200us
    | RTSpecialName = 0x400us
    | HasDefault = 0x1000us
    | Reserved = 0xE9FFus

/// <summary>(0x17) Represents a row in the <c>Property</c> table (II.22.34).</summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type PropertyRow =
    { Flags: PropertyFlags
      Name: IdentifierOffset
      Type: PropertySigOffset }

    member this.Equals other = this.Name === other.Name && this.Type === other.Type

    override this.GetHashCode() = HashCode.Combine(this.Name, this.Type)

    override this.Equals obj =
        match obj with
        | :? PropertyRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<PropertyRow> with member this.Equals other = this.Equals(other = other)

/// <summary>
/// (0x15) Represents a row in the <c>PropertyMap</c> table, which specifies the properties associated with a type (II.22.35).
/// </summary>
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type PropertyMapRow =
    { Parent: TableIndex<TypeDefRow>
      PropertyList: TableIndex<PropertyRow> }
    interface ITableRow

/// <summary>Describes what kind of method is associated with a <c>Property</c> or <c>Event</c> (II.23.1.12).</summary>
[<Flags>]
type MethodSemanticsFlags =
    /// The method is a setter for a property.
    | Setter = 1us
    /// The method is a getter for a property.
    | Getter = 2us
    | Other = 4us
    /// The method is used to subscribe to an event.
    | AddOn = 8us
    /// The method is used to unsubscribe to an event.
    | RemoveOn = 0x10us
    /// <summary>The method raises an event, corresponds to the optional <c>raise_</c> method.</summary>
    | Fire = 0x20us

/// <summary>
/// (0x18) Represents a row in the <c>MethodSemantics</c> table, which describes which methods are associated with which
/// properties or events (II.22.28).
/// </summary>
[<IsReadOnly; Struct>]
type MethodSemanticsRow =
    { Semantics: MethodSemanticsFlags
      Method: TableIndex<MethodDefRow>
      Association: HasSemantics }
    interface ITableRow

/// <summary>(0x19) Represents a row in the <c>MethodImpl</c> table (II.22.27).</summary>
/// <remarks>Rows in this table are generated by the C# compiler for explicit interface implementations.</remarks>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type MethodImplRow =
    { Class: TableIndex<TypeDefRow>
      MethodBody: MethodDefOrRef
      MethodDeclaration: MethodDefOrRef }

    member this.Equals other = this.Class === other.Class && this.MethodDeclaration === other.MethodDeclaration

    override this.GetHashCode() = HashCode.Combine(this.Class, this.MethodDeclaration)

    override this.Equals obj =
        match obj with
        | :? MethodImplRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<MethodImplRow> with member this.Equals other = this.Equals(other = other)

/// <summary>(0x1A) Represents a row in the <c>ModuleRef</c> table (II.22.31).</summary>
[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
[<StructuralComparison; StructuralEquality>]
type ModuleRefRow = { Name: IdentifierOffset } interface ITableRow

/// <summary>
/// (0x1B) Represents a row in the <c>TypeSpec</c> table, which contains an offset into the <c>#Blob</c> heap pointing to a
/// <c>TypeSpec</c> item (II.22.39).
/// </summary>
/// <remarks>Rows in this table are generated by the C# and F# compilers for generic instantiations of types.</remarks>
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type TypeSpecRow =
    internal { TypeSpec: BlobOffset }
    member this.Signature = this.TypeSpec
    interface ITableRow



/// Specifies where the initial value of a field is stored (II.22.18).
[<IsReadOnly>]
type FieldValueLocation = struct
    val Value: uint32
    internal new (value) = { Value = value }
end

/// <summary>
/// (0x1D) Represents a row in the <c>FieldRVA</c> table, which contains a Relative Virtual Address specifying the initial value
/// of a field (II.22.18).
/// </summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type FieldRvaRow =
    { Rva: FieldValueLocation
      Field: TableIndex<FieldRow> }

    member this.Equals other = this.Field === other.Field

    override this.GetHashCode() = this.Field.GetHashCode()

    override this.Equals obj =
        match obj with
        | :? FieldRvaRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<FieldRvaRow> with member this.Equals other = this.Equals(other = other)

[<System.Runtime.CompilerServices.IsReadOnly>]
type AssemblyVersion = struct // TODO: Update row types for Assembly and AssemblyRef to use this struct.
    val Major: uint16
    val Minor: uint16
    val Build: uint16
    val Revision: uint16

    new(major, minor, build, revision) = { Major = major; Minor = minor; Build = build; Revision = revision }
end

/// <summary>Specifies the algorithm used to compute the hash for the contents of an assembly (II.23.1.1).</summary>
type AssemblyHashAlgorithm =
    | None = 0u
    | MD5 = 0x8003u
    | SHA1 = 0x8004u

/// <summary>Describes the attributes of an <c>Assembly</c> (II.23.1.2).</summary>
[<Flags>]
type AssemblyFlags =
    | None = 0u
    | PublicKey = 1u
    | Retargetable = 0x100u
    | DisableJitCompileOptimizer = 0x4000u
    | EnableJitCompileTracking = 0x8000u

/// <summary>(0x20) Represents the row in the <c>Assembly</c> table, which describes the current assembly (II.22.2).</summary>
[<IsReadOnly; Struct>]
type AssemblyRow =
    { HashAlgId: AssemblyHashAlgorithm
      MajorVersion: uint16
      MinorVersion: uint16
      BuildNumber: uint16
      RevisionNumber: uint16
      Flags: AssemblyFlags
      PublicKey: BlobOffset // TODO: Special type for PublicKey
      Name: FileNameOffset
      Culture: StringOffset }

    member this.Version =
        Version(int32 this.MajorVersion, int32 this.MinorVersion, int32 this.BuildNumber, int32 this.RevisionNumber)

    interface ITableRow

/// <summary>
/// (0x23) Represents a row in the <c>AssemblyRef</c> table, which describes the assemblies referenced by the current assembly or
/// module (II.22.5).
/// </summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type AssemblyRefRow =
    { MajorVersion: uint16
      MinorVersion: uint16
      BuildNumber: uint16
      RevisionNumber: uint16
      PublicKeyOrToken: PublicKeyOrTokenOffset
      Name: FileNameOffset
      Culture: StringOffset
      HashValue: BlobOffset }

    member this.Version =
        Version(int32 this.MajorVersion, int32 this.MinorVersion, int32 this.BuildNumber, int32 this.RevisionNumber)

    member this.Flags = if this.PublicKeyOrToken.IsPublicKey then AssemblyFlags.PublicKey else AssemblyFlags.None

    member this.Equals other =
        this.MajorVersion = other.MajorVersion
        && this.MinorVersion = other.MinorVersion
        && this.BuildNumber = other.BuildNumber
        && this.RevisionNumber = other.RevisionNumber
        && this.PublicKeyOrToken === other.PublicKeyOrToken
        && this.Name === other.Name
        && this.Culture === other.Culture

    override this.GetHashCode() =
        HashCode.Combine (
            this.MajorVersion,
            this.MinorVersion,
            this.BuildNumber,
            this.RevisionNumber,
            this.PublicKeyOrToken,
            this.Name,
            this.Culture
        )

    override this.Equals obj =
        match obj with
        | :? AssemblyRefRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<AssemblyRefRow> with member this.Equals other = this.Equals(other = other)



/// <summary>Specifies the contents of a <c>File</c> (II.23.1.6).</summary>
type FileFlags =
    | ContainsMetadata = 0u
    | ContainsNoMetadata = 1u

/// <summary>(0x26) Represents a row in the <c>File</c> table (II.22.19).</summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type FileRow =
    { Flags: FileFlags
      Name: FileNameOffset
      HashValue: BlobOffset }

    member this.Equals other = this.Name === other.Name

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals obj =
        match obj with
        | :? FileRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<FileRow> with member this.Equals other = this.Equals other



/// <summary>Describes the visibility of a <c>ManifestResource</c> (II.23.1.9).</summary>
[<Flags>]
type ManifestResourceFlags =
    | Public = 1u
    | Private = 2u
    | VisibilityMask = 7u

/// <summary>(0x28) Represents a row in the <c>ManifestResource</c> table (II.22.24).</summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type ManifestResourceRow =
    { Offset: uint32
      Flags: ManifestResourceFlags
      Name: IdentifierOffset
      Implementation: Implementation }

    member this.Equals other = this.Name === other.Name

    override this.GetHashCode() = this.Name.GetHashCode()

    override this.Equals obj =
        match obj with
        | :? ManifestResourceRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<ManifestResourceRow> with member this.Equals other = this.Equals other

/// <summary>
/// (0x29) Represents a row in the <c>NestedClass</c> table, which specifies which types are nested inside other types in this
/// assembly (II.22.32).
/// </summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type NestedClassRow =
    { NestedClass: TableIndex<TypeDefRow>
      EnclosingClass: TableIndex<TypeDefRow> }

    member this.Equals other = this.NestedClass === other.NestedClass

    override this.GetHashCode() = this.NestedClass.GetHashCode()

    override this.Equals obj =
        match obj with
        | :? NestedClassRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<NestedClassRow> with member this.Equals other = this.Equals other

/// <summary>
/// Specifies the attributes of a <c>GenericParam</c>, such as its variance and special constraints (II.23.1.7).
/// </summary>
[<Flags>]
type GenericParamFlags =
    | None = 0us
    | Covariant = 1us
    | Contravariant = 2us
    | VarianceMask = 3us
    | SpecialConstraintMask = 0x1Cus
    /// <summary>Corresponds to the <c>class</c> constraint, which constrains the type to being a reference type.</summary>
    | ReferenceTypeConstraint = 4us
    /// <summary>
    /// Corresponds to the <c>valuetype</c> constraint, which constrains the type to being a value type that is not
    /// <see cref="T:System.Nullable`1"/>.
    /// </summary>
    | NotNullableValueTypeConstraint = 8us
    /// <summary>
    /// Corresponds to the <c>.ctor</c> constraint, which constrains the type to being a concrete reference type with a public
    /// constructor taking no arguments, or any value type.
    /// </summary>
    | DefaultConstructorConstraint = 0x10us

/// <summary>
/// (0x2A) Represents a row in the <c>GenericParam</c> table, which describes a generic parameter of a type or method (II.22.20).
/// </summary>
[<IsReadOnly; Struct>]
[<NoComparison; CustomEquality>]
type GenericParamRow =
    { Number: uint16
      Flags: GenericParamFlags
      Owner: TypeOrMethodDef
      Name: IdentifierOffset }

    member this.Equals other = this.Owner === other.Owner && (this.Number = other.Number || this.Name === other.Name)

    override this.GetHashCode() = HashCode.Combine(this.Owner, this.Number)

    override this.Equals obj =
        match obj with
        | :? GenericParamRow as other -> this.Equals other
        | _ -> false

    interface ITableRow
    interface IEquatable<GenericParamRow> with member this.Equals other = this.Equals other

/// <summary>(0x2B) Represents a row in the <c>MethodSpec</c> table (II.22.29).</summary>
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type MethodSpecRow =
    { Method: MethodDefOrRef
      Instantiation: MethodSpecOffset }
    interface ITableRow

/// <summary>
/// (0x2C) Represents a row in the <c>GenericParamConstraint</c> table, which specifies the interfaces and/or base type that a
/// generic parameter is constrained to implement or derive from (II.22.21).
/// </summary>
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type GenericParamConstraintRow =
    { Owner: TableIndex<GenericParamRow>
      Constraint: TypeDefOrRef }
    interface ITableRow
