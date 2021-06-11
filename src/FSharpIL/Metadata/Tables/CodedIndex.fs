namespace FSharpIL.Metadata.Tables

/// Represents a coded index, which represents an index into one of many possible multiple metadata tables (II.24.2.6).
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type CodedIndex<'Tag when 'Tag : enum<uint8>> = struct
    val Tag: 'Tag
    val Index: uint32
    internal new (tag, index) = { Tag = tag; Index = index }
    member this.IsNull = this.Index = 0u
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

// TODO: Create "Factory" type for coded indices that stores the flags corresponding to the tables that it uses, and the number of encoding bits since this information is used in writing and reading.
// type CodedIndexKind<'Tag when 'Tag : enum<uint8>> = struct
