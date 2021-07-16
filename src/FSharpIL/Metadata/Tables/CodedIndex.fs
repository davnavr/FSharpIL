namespace FSharpIL.Metadata.Tables

open System
open System.Runtime.CompilerServices

/// Represents a coded index, which represents an index into one of many possible multiple metadata tables (II.24.2.6).
[<IsReadOnly; Struct>]
[<CustomComparison; CustomEquality>]
type CodedIndex<'Tag when 'Tag : enum<uint8> and 'Tag : comparison> = struct
    val Tag: 'Tag
    val Index: uint32

    internal new (tag, index) = { Tag = tag; Index = index }

    member this.IsNull = this.Index = 0u

    member this.Equals(other: CodedIndex<'Tag>) = // TODO: Fix, comparison of tag here causes boxing.
        LanguagePrimitives.EnumToValue this.Tag = LanguagePrimitives.EnumToValue other.Tag && this.Index = other.Index

    member this.CompareTo(other: CodedIndex<'Tag>) =
        match compare this.Index other.Index with
        | 0 -> compare (LanguagePrimitives.EnumToValue this.Tag) (LanguagePrimitives.EnumToValue other.Tag)
        | result -> result

    override this.Equals obj =
        match obj with
        | :? CodedIndex<'Tag> as other -> this.Equals(other = other)
        | _ -> false

    override this.GetHashCode() = HashCode.Combine(this.Tag, this.Index)

    interface IEquatable<CodedIndex<'Tag>> with member this.Equals other = this.Equals(other = other)
    interface IComparable<CodedIndex<'Tag>> with member this.CompareTo other = this.CompareTo other
    interface IComparable with member this.CompareTo obj = this.CompareTo(obj :?> CodedIndex<'Tag>)
end

type TypeDefOrRefTag =
    | TypeDef = 0uy
    | TypeRef = 1uy
    | TypeSpec = 2uy

type HasConstantTag =
    | Field = 0uy
    | Param = 1uy
    | Property = 2uy

type HasCustomAttributeTag =
    | MethodDef = 0uy
    | Field = 1uy
    | TypeRef = 2uy
    | TypeDef = 3uy
    | Param = 4uy
    | InterfaceImpl = 5uy
    | MemberRef = 6uy
    | Module = 7uy
    | Permission = 8uy
    | Property = 9uy
    | Event = 10uy
    | StandAloneSig = 11uy
    | ModuleRef = 12uy
    | TypeSpec = 13uy
    | Assembly = 14uy
    | AssemblyRef = 15uy
    | File = 16uy
    | ExportedType = 17uy
    | ManifestResource = 18uy
    | GenericParam = 19uy
    | GenericParamConstraint = 20uy
    | MethodSpec = 21uy

type HasFieldMarshalTag =
    | Field = 0uy
    | Param = 1uy

type HasDeclSecurityTag =
    | TypeDef = 0uy
    | MethodDef = 1uy
    | Assembly = 2uy

type MemberRefParentTag =
    | TypeDef = 0uy
    | TypeRef = 1uy
    | ModuleRef = 2uy
    | MethodDef = 3uy
    | TypeSpec = 4uy

type HasSemanticsTag =
    | Event = 0uy
    | Property = 1uy

type MethodDefOrRefTag =
    | MethodDef = 0uy
    | MemberRef = 1uy

type MemberForwardedTag =
    | Field = 0uy
    | MethodDef = 1uy

type ImplementationTag =
    | File = 0uy
    | AssemblyRef = 1uy
    | ExportedType = 2uy

type CustomAttributeTypeTag =
    | MethodDef = 2uy
    | MemberRef = 3uy

type ResolutionScopeTag =
    | Module = 0uy
    | ModuleRef = 1uy
    | AssemblyRef = 2uy
    | TypeRef = 3uy

type TypeOrMethodDefTag =
    | TypeDef = 0uy
    | MethodDef = 1uy

type TypeDefOrRef = CodedIndex<TypeDefOrRefTag>
type HasConstant = CodedIndex<HasConstantTag>
type HasCustomAttribute = CodedIndex<HasCustomAttributeTag>
type HasFieldMarshal = CodedIndex<HasFieldMarshalTag>
type HasDeclSecurity = CodedIndex<HasDeclSecurityTag>
type MemberRefParent = CodedIndex<MemberRefParentTag>
type HasSemantics = CodedIndex<HasSemanticsTag>
type MethodDefOrRef = CodedIndex<MethodDefOrRefTag>
type MemberForwarded = CodedIndex<MemberForwardedTag>
type Implementation = CodedIndex<ImplementationTag>
type CustomAttributeType = CodedIndex<CustomAttributeTypeTag>
type ResolutionScope = CodedIndex<ResolutionScopeTag>
type TypeOrMethodDef = CodedIndex<TypeOrMethodDefTag>

/// Describes the possible tables that a coded index can point to and the number of bits needed to encode its tag.
[<IsReadOnly>]
type CodedIndexKind<'Tag when 'Tag : enum<uint8>> = struct
    val PossibleTables: ValidTableFlags
    val NumEncodingBits: int32
    val internal MaxSmallIndex: uint32
    /// <param name="tables">A bit mask specifying the possible tables that the coded index can point to.</param>
    /// <param name="n">The number of bits needed to encode the tag.</param>
    internal new (tables, n) =
        { PossibleTables = tables
          NumEncodingBits = n
          MaxSmallIndex = 0xFFFFu >>> n }

    member this.IsLarge(counts: ITableRowCounts)=
           let mutable large, n = false, 0
           while not large && n < 64 do
               let table = ValidTableFlags.Module <<< n
               large <- table &&& this.PossibleTables <> ValidTableFlags.None && counts.RowCount table > this.MaxSmallIndex
               n <- n + 1
           large
end

[<AbstractClass; Sealed>]
type CodedIndexKinds private () =
    static let typeDefOrRef =
        CodedIndexKind<TypeDefOrRefTag> (
            ValidTableFlags.TypeDef
            ||| ValidTableFlags.TypeRef
            ||| ValidTableFlags.TypeSpec,
            2
        )

    static let hasConstant =
        CodedIndexKind<HasConstantTag>(ValidTableFlags.Field ||| ValidTableFlags.Param ||| ValidTableFlags.Property, 2)

    static let hasCustomAttribute =
        CodedIndexKind<HasCustomAttributeTag> (
            ValidTableFlags.MethodDef
            ||| ValidTableFlags.Field
            ||| ValidTableFlags.TypeRef
            ||| ValidTableFlags.TypeDef
            ||| ValidTableFlags.Param
            ||| ValidTableFlags.InterfaceImpl
            ||| ValidTableFlags.MemberRef
            ||| ValidTableFlags.Module
            // ||| ValidTableFlags.Permission
            ||| ValidTableFlags.Property
            ||| ValidTableFlags.Event
            ||| ValidTableFlags.StandAloneSig
            ||| ValidTableFlags.ModuleRef
            ||| ValidTableFlags.TypeSpec
            ||| ValidTableFlags.Assembly
            ||| ValidTableFlags.AssemblyRef
            ||| ValidTableFlags.File
            ||| ValidTableFlags.ExportedType
            ||| ValidTableFlags.ManifestResource
            ||| ValidTableFlags.GenericParam
            ||| ValidTableFlags.GenericParamConstraint
            ||| ValidTableFlags.MethodSpec,
            5
        )

    static let hasFieldMarshal = CodedIndexKind<HasFieldMarshalTag>(ValidTableFlags.Field ||| ValidTableFlags.Param, 1)

    static let hasDeclSecurity = CodedIndexKind<HasDeclSecurityTag>(ValidTableFlags.TypeDef ||| ValidTableFlags.MethodDef ||| ValidTableFlags.Assembly, 2)

    static let memberRefParent =
        CodedIndexKind<MemberRefParentTag> (
            ValidTableFlags.TypeDef
            ||| ValidTableFlags.TypeRef
            ||| ValidTableFlags.ModuleRef
            ||| ValidTableFlags.MethodDef
            ||| ValidTableFlags.TypeSpec,
            3
        )

    static let hasSemantics = CodedIndexKind<HasSemanticsTag>(ValidTableFlags.Event ||| ValidTableFlags.Property, 1)

    static let methodDefOrRef = CodedIndexKind<MethodDefOrRefTag>(ValidTableFlags.MethodDef ||| ValidTableFlags.MemberRef, 1)

    static let memberForwarded = CodedIndexKind<MemberForwardedTag>(ValidTableFlags.Field ||| ValidTableFlags.MethodDef, 1)

    static let implementation =
        CodedIndexKind<ImplementationTag> (
            ValidTableFlags.File
            ||| ValidTableFlags.AssemblyRef
            ||| ValidTableFlags.ExportedType,
            2
        )

    static let customAttributeType = CodedIndexKind<CustomAttributeTypeTag>(ValidTableFlags.MethodDef ||| ValidTableFlags.MemberRef, 3)

    static let resolutionScope =
        CodedIndexKind<ResolutionScopeTag> (
            ValidTableFlags.Module
            ||| ValidTableFlags.ModuleRef
            ||| ValidTableFlags.AssemblyRef
            ||| ValidTableFlags.TypeRef,
            2
        )

    static let typeOrMethodDef = CodedIndexKind<TypeOrMethodDefTag>(ValidTableFlags.TypeDef ||| ValidTableFlags.MethodDef, 1)

    static member TypeDefOrRef = &typeDefOrRef
    static member HasConstant = &hasConstant
    static member HasCustomAttribute = &hasCustomAttribute
    static member HasFieldMarshal = &hasFieldMarshal
    static member HasDeclSecurity = &hasDeclSecurity
    static member MemberRefParent = &memberRefParent
    static member HasSemantics = &hasSemantics
    static member MethodDefOrRef = &methodDefOrRef
    static member MemberForwarded = &memberForwarded
    static member Implementation = &implementation
    static member CustomAttributeType = &customAttributeType
    static member ResolutionScope = &resolutionScope
    static member TypeOrMethodDef = &typeOrMethodDef
