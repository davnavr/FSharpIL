namespace FSharpIL.Reading

open System
open System.Collections.Generic
open System.Configuration.Assemblies
open System.Runtime.CompilerServices
open System.Reflection

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Reading.ByteParser

// TODO: Make this a struct instead.
[<IsReadOnly; IsByRefLike; Struct>]
type internal IndexParser (table: MetadataTableFlags) =
    member _.Length(counts: MetadataTableCounts) =
        if counts.GetValueOrDefault table <= 0xFFFFu
        then 2
        else 4
    member this.Parse(counts, offset, buffer: Span<byte>) =
        let buffer' = buffer.Slice(offset, this.Length counts)
        match buffer'.Length with
        | 2 -> uint32(Bytes.readU2 0 buffer')
        | 4
        | _ -> Bytes.readU4 0 buffer'

[<RequireQualifiedAccess>]
module internal IndexParser =
    let inline length table counts = IndexParser(table).Length counts
    let inline parse table counts offset buffer = IndexParser(table).Parse(counts, offset, buffer)

[<RequireQualifiedAccess>]
module internal Offset =
    let parse (buffer: Span<byte>) =
        match buffer.Length with
        | 4 -> Bytes.readU4 0 buffer
        | 2 -> uint32(Bytes.readU2 0 buffer)
        | bad -> invalidArg "buffer" (sprintf "Invalid buffer length %i, expected length of 2 or 4" bad)

[<IsReadOnly; Struct>]
type internal StringParser (sizes: HeapSizes) =
    interface IByteParser<ParsedString> with
        member _.Parse buffer = { ParsedString.StringOffset = Offset.parse buffer }
        member _.Length = sizes.StringSize

[<IsReadOnly; Struct>]
type internal GuidParser (sizes: HeapSizes) =
    interface IByteParser<ParsedGuid> with
        member _.Parse buffer = { ParsedGuid.GuidOffset = Offset.parse buffer }
        member _.Length = sizes.GuidSize

[<IsReadOnly; Struct>]
type internal BlobParser (sizes: HeapSizes) =
    interface IByteParser<ParsedBlob> with
        member _.Parse buffer = { BlobOffset = Offset.parse buffer }
        member _.Length = sizes.BlobSize

// TODO: Allow usage of existing types in FSharpIL.Metadata by using generic parameters and remove "string" and GUID and using some sort of index and builder system just like with Blobs.

[<IsReadOnly; Struct>]
type ParsedModuleRow =
    { Generation: uint16
      Name: ParsedString
      Mvid: ParsedGuid
      EncId: ParsedGuid
      EncBaseId: ParsedGuid }

[<IsReadOnly; Struct>]
type ModuleParser (sizes: HeapSizes) =
    interface IByteParser<ParsedModuleRow> with
        member _.Parse buffer =
            let guid = GuidParser sizes
            let goffset = 2 + sizes.StringSize
            { Generation = Bytes.readU2 0 buffer
              Name = parse 2 buffer (StringParser sizes)
              Mvid = parse goffset buffer guid
              EncId = parse (goffset + sizes.GuidSize) buffer guid
              EncBaseId = parse (goffset + (2 * sizes.GuidSize)) buffer guid }
        member _.Length = 2 + sizes.StringSize + (3 * sizes.GuidSize)

[<IsReadOnly; Struct>]
type internal ParsedCodedIndex =
    { Tag: uint8; Index: uint32 }
    member this.IsNull = this.Index = 0u

[<RequireQualifiedAccess>]
module CodedIndex =
    [<IsReadOnly; IsByRefLike; Struct>]
    type Parser = struct
        /// Gets a value indicating whether this coded index would occupy four bytes.
        val IsLarge: bool
        val EncodingBits: int32
        internal new (count: uint32, n: int32) = { IsLarge = count > (0xFFFFu >>> n); EncodingBits = n }
        /// The number of bytes that this coded index would occupy.
        member this.Length = if this.IsLarge then 4 else 2
        member internal this.Parse(buffer: Span<byte>) =
            if this.IsLarge then
                let index = Bytes.readU4 0 buffer
                let filter = UInt32.MaxValue <<< this.EncodingBits
                { Tag = uint8(index &&& ~~~filter)
                  Index = (index &&& filter) >>> this.EncodingBits } // TODO: Shift index
            else
                let index = Bytes.readU2 0 buffer
                let filter = UInt16.MaxValue <<< this.EncodingBits
                { Tag = uint8(index &&& ~~~filter)
                  Index = uint32(index &&& filter) >>> this.EncodingBits }
        member inline internal this.Parse(offset, buffer: Span<byte>) = this.Parse(buffer.Slice offset)
    end

    let resolutionScopeParser (counts: MetadataTableCounts) =
        Parser (
            counts.GetValueOrDefault MetadataTableFlags.Module
            + (counts.GetValueOrDefault MetadataTableFlags.ModuleRef)
            + (counts.GetValueOrDefault MetadataTableFlags.AssemblyRef)
            + (counts.GetValueOrDefault MetadataTableFlags.TypeRef),
            2
        )

    let typeDefOrRefOrSpec (counts: MetadataTableCounts) =
        Parser (
            counts.GetValueOrDefault MetadataTableFlags.TypeDef
            + (counts.GetValueOrDefault MetadataTableFlags.TypeRef)
            + (counts.GetValueOrDefault MetadataTableFlags.TypeSpec),
            2
        )

    let memberRefParent (counts: MetadataTableCounts) =
        Parser (
            counts.GetValueOrDefault MetadataTableFlags.TypeDef
            + (counts.GetValueOrDefault MetadataTableFlags.TypeRef)
            + (counts.GetValueOrDefault MetadataTableFlags.ModuleRef)
            + (counts.GetValueOrDefault MetadataTableFlags.MethodDef)
            + (counts.GetValueOrDefault MetadataTableFlags.TypeSpec),
            3
        )

    let hasConstant (counts: MetadataTableCounts) =
        Parser (
            counts.GetValueOrDefault MetadataTableFlags.Field
            + (counts.GetValueOrDefault MetadataTableFlags.Param)
            + (counts.GetValueOrDefault MetadataTableFlags.Property),
            2
        )

    let hasCustomAttribute (counts: MetadataTableCounts) =
        Parser (
            counts.GetValueOrDefault MetadataTableFlags.MethodDef
            + (counts.GetValueOrDefault MetadataTableFlags.Field)
            + (counts.GetValueOrDefault MetadataTableFlags.TypeRef)
            + (counts.GetValueOrDefault MetadataTableFlags.TypeDef)
            + (counts.GetValueOrDefault MetadataTableFlags.Param)
            + (counts.GetValueOrDefault MetadataTableFlags.InterfaceImpl)
            + (counts.GetValueOrDefault MetadataTableFlags.MemberRef)
            + (counts.GetValueOrDefault MetadataTableFlags.Module)
            //+ (counts.GetValueOrDefault MetadataTableFlags.Permission)
            + (counts.GetValueOrDefault MetadataTableFlags.Property)
            + (counts.GetValueOrDefault MetadataTableFlags.Event)
            + (counts.GetValueOrDefault MetadataTableFlags.StandAloneSig)
            + (counts.GetValueOrDefault MetadataTableFlags.ModuleRef)
            + (counts.GetValueOrDefault MetadataTableFlags.TypeSpec)
            + (counts.GetValueOrDefault MetadataTableFlags.Assembly)
            + (counts.GetValueOrDefault MetadataTableFlags.AssemblyRef)
            + (counts.GetValueOrDefault MetadataTableFlags.File)
            + (counts.GetValueOrDefault MetadataTableFlags.ExportedType)
            + (counts.GetValueOrDefault MetadataTableFlags.ManifestResource)
            + (counts.GetValueOrDefault MetadataTableFlags.GenericParam)
            + (counts.GetValueOrDefault MetadataTableFlags.GenericParamConstraint)
            + (counts.GetValueOrDefault MetadataTableFlags.MethodSpec),
            5
        )

    let customAttributeType (counts: MetadataTableCounts) =
        Parser (
            counts.GetValueOrDefault MetadataTableFlags.MethodDef + (counts.GetValueOrDefault MetadataTableFlags.MemberRef),
            3
        )

    let hasSemantics (counts: MetadataTableCounts) =
        Parser (
            counts.GetValueOrDefault MetadataTableFlags.Event + (counts.GetValueOrDefault MetadataTableFlags.Property),
            1
        )

    let methodDefOrRef (counts: MetadataTableCounts) =
        Parser (
            counts.GetValueOrDefault MetadataTableFlags.MethodDef + (counts.GetValueOrDefault MetadataTableFlags.MemberRef),
            1
        )

    let implementation (counts: MetadataTableCounts) =
        Parser (
            counts.GetValueOrDefault MetadataTableFlags.File
            + (counts.GetValueOrDefault MetadataTableFlags.AssemblyRef)
            + (counts.GetValueOrDefault MetadataTableFlags.ExportedType),
            2
        )

    let typeOrMethodDef (counts: MetadataTableCounts) =
        Parser (
            counts.GetValueOrDefault MetadataTableFlags.TypeDef + (counts.GetValueOrDefault MetadataTableFlags.MethodDef),
            1
        )

// TODO: Have constants that store the tags for coded indices instead of duplicating them with the writing code.
type [<IsReadOnly; Struct>] ParsedResolutionScope = private { ResolutionScope: ParsedCodedIndex }

// TODO: Use RawIndex<'T> type instead of uint32.

[<RequireQualifiedAccess>]
module ParsedResolutionScope =
    let (|Null|Module|ModuleRef|AssemblyRef|TypeRef|Unknown|) { ResolutionScope = rscope } =
        match rscope with
        | { Index = 0u } -> Null
        | { Tag = 0uy } -> Module rscope.Index
        | { Tag = 1uy } -> ModuleRef rscope.Index
        | { Tag = 2uy } -> AssemblyRef rscope.Index
        | { Tag = 3uy } -> TypeRef rscope.Index
        | { Tag = unknown } -> Unknown(unknown, rscope.Index)

[<IsReadOnly; Struct>]
type ParsedTypeRefRow =
    { ResolutionScope: ParsedResolutionScope
      TypeName: ParsedString
      TypeNamespace: ParsedString }

[<IsReadOnly; Struct>]
type TypeRefParser (sizes: HeapSizes, counts: MetadataTableCounts) =
    member inline private _.ResolutionScope = CodedIndex.resolutionScopeParser counts
    interface IByteParser<ParsedTypeRefRow> with
        member this.Parse buffer =
            let str = StringParser sizes
            let rscope = this.ResolutionScope
            { ResolutionScope = { ResolutionScope = rscope.Parse buffer }
              TypeName = parse rscope.Length buffer str
              TypeNamespace = parse (rscope.Length + sizes.StringSize) buffer str }
        member this.Length = this.ResolutionScope.Length + (2 * sizes.StringSize)

[<IsReadOnly; Struct>]
type ParsedExtends =
    private { Extends: ParsedCodedIndex }
    member this.Null = this.Extends.IsNull

type [<IsReadOnly; Struct>] ParsedTypeDefOrRefOrSpec = private { Tag: TypeDefOrRefOrSpecTag; TypeIndex: uint32 }

[<RequireQualifiedAccess>]
module ParsedTypeDefOrRefOrSpec =
    let (|TypeDef|TypeRef|TypeSpec|Unknown|) { Tag = tag; TypeIndex = i } =
        match tag with
        | TypeDefOrRefOrSpecTag.Def -> TypeDef i
        | TypeDefOrRefOrSpecTag.Ref -> TypeRef i
        | TypeDefOrRefOrSpecTag.Spec -> TypeSpec i
        | _ -> Unknown(tag, i)

[<RequireQualifiedAccess>]
module ParsedExtends =
    let (|Null|TypeDef|TypeRef|TypeSpec|Unknown|) { ParsedExtends.Extends = extends } =
        match extends with
        | { Index = 0u } -> Null
        | { Tag = 0uy } -> TypeDef extends.Index
        | { Tag = 1uy } -> TypeRef extends.Index
        | { Tag = 2uy } -> TypeSpec extends.Index
        | { Tag = unknown } -> Unknown(unknown, extends.Index)
    let toTypeDefOrRefOrSpec extends =
        match extends with
        | Null -> ValueNone
        | { Extends = { Tag = tag; Index = index } } -> ValueSome { Tag = LanguagePrimitives.EnumOfValue tag; TypeIndex = index }

[<IsReadOnly; Struct>]
type ParsedTypeDefRow =
    { Flags: TypeAttributes
      TypeName: ParsedString
      TypeNamespace: ParsedString
      Extends: ParsedExtends
      FieldList: uint32
      MethodList: uint32 }

[<IsReadOnly; Struct>]
type TypeDefParser (sizes: HeapSizes, counts: MetadataTableCounts) =
    member inline private _.Extends = CodedIndex.typeDefOrRefOrSpec counts
    interface IByteParser<ParsedTypeDefRow> with
        member this.Parse buffer =
            let str = StringParser sizes
            let extends = this.Extends
            let eoffset = 4 + (2 * sizes.StringSize)
            let field = IndexParser MetadataTableFlags.Field
            let f = field.Parse(counts, eoffset + extends.Length, buffer)
            { Flags = LanguagePrimitives.EnumOfValue(int32(Bytes.readU4 0 buffer))
              TypeName = parse 4 buffer str
              TypeNamespace = parse (4 + sizes.StringSize) buffer str
              Extends = { Extends = extends.Parse(eoffset, buffer) }
              FieldList = field.Parse(counts, eoffset + extends.Length, buffer)
              MethodList =
                IndexParser.parse
                    MetadataTableFlags.MethodDef
                    counts
                    (eoffset + extends.Length + field.Length counts) buffer }
        member this.Length =
            4 + (2 * sizes.StringSize) + this.Extends.Length
            + (IndexParser.length MetadataTableFlags.Field counts)
            + (IndexParser.length MetadataTableFlags.MethodDef counts)

[<IsReadOnly; Struct>]
type ParsedFieldRow =
    { Flags: FieldAttributes
      Name: ParsedString
      Signature: ParsedFieldSig }

type FieldParser (sizes: HeapSizes) =
    interface IByteParser<ParsedFieldRow> with
        member _.Parse buffer =
            { Flags = LanguagePrimitives.EnumOfValue(int32(Bytes.readU2 0 buffer))
              Name = parse 2 buffer (StringParser sizes)
              Signature = { FieldSig = parse (2 + sizes.StringSize) buffer (BlobParser sizes) } }
        member _.Length = 2 + sizes.StringSize + sizes.BlobSize

[<IsReadOnly; Struct>]
type ParsedMethodRow =
    { Rva: uint32
      ImplFlags: MethodImplAttributes
      Flags: MethodAttributes
      Name: ParsedString
      Signature: ParsedMethodDefSig
      ParamList: uint32 }

[<IsReadOnly; Struct>]
type MethodDefParser (sizes: HeapSizes, counts: MetadataTableCounts) =
    interface IByteParser<ParsedMethodRow> with
        member _.Parse buffer =
            { Rva = Bytes.readU4 0 buffer
              ImplFlags = LanguagePrimitives.EnumOfValue(int32(Bytes.readU2 4 buffer))
              Flags = LanguagePrimitives.EnumOfValue(int32(Bytes.readU2 6 buffer))
              Name = parse 8 buffer (StringParser sizes)
              Signature = { MethodDefSig = parse (8 + sizes.StringSize) buffer (BlobParser sizes) }
              ParamList = IndexParser.parse MetadataTableFlags.Param counts (8 + sizes.StringSize + sizes.BlobSize) buffer }
        member _.Length = 8 + sizes.StringSize + sizes.BlobSize + (IndexParser.length MetadataTableFlags.Param counts)

[<IsReadOnly; Struct>]
type ParsedParamRow =
    { Flags: ParameterAttributes
      Sequence: uint16
      Name: ParsedString }

[<IsReadOnly; Struct>]
type ParamParser (sizes: HeapSizes) =
    interface IByteParser<ParsedParamRow> with
        member _.Parse buffer =
            { Flags = LanguagePrimitives.EnumOfValue(int32(Bytes.readU2 0 buffer))
              Sequence = Bytes.readU2 2 buffer
              Name = parse 4 buffer (StringParser sizes) }
        member _.Length = 4 + sizes.StringSize

[<IsReadOnly; Struct>]
type ParsedInterfaceImpl = { Class: uint32; Interface: ParsedTypeDefOrRefOrSpec }

[<IsReadOnly; Struct>]
type InterfaceImplParser (counts: MetadataTableCounts) =
    member inline private _.Class = IndexParser MetadataTableFlags.TypeDef
    member inline private _.Interface = CodedIndex.typeDefOrRefOrSpec counts
    interface IByteParser<ParsedInterfaceImpl> with
        member this.Parse buffer =
            let parent = this.Class
            let { Tag = tag; Index = i } = this.Interface.Parse(parent.Length counts, buffer)
            { Class = parent.Parse(counts, 0, buffer)
              Interface = { TypeIndex = i; Tag = LanguagePrimitives.EnumOfValue tag } }
        member this.Length = this.Class.Length counts + this.Interface.Length

type [<IsReadOnly; Struct>] ParsedMemberRefParent = private { MemberRefParent: ParsedCodedIndex }

[<IsReadOnly; Struct>]
type ParsedMemberRef =
    { Class: ParsedMemberRefParent
      Name: ParsedString
      Signature: ParsedMemberRefSig }

[<IsReadOnly; Struct>]
type MemberRefParser (sizes: HeapSizes, counts: MetadataTableCounts) =
    member inline private _.Class = CodedIndex.memberRefParent counts
    interface IByteParser<ParsedMemberRef> with
        member this.Parse buffer =
            let parent = this.Class
            { Class = { MemberRefParent = parent.Parse buffer }
              Name = parse parent.Length buffer (StringParser sizes)
              Signature = { MemberRefSig = parse (parent.Length + sizes.StringSize) buffer (BlobParser sizes) } }
        member this.Length = this.Class.Length + sizes.StringSize + sizes.BlobSize

type [<IsReadOnly; Struct>] ParsedConstantParent = private { HasConstant: ParsedCodedIndex }

[<IsReadOnly; Struct>]
type ParsedConstant =
    { Type: ElementType
      Parent: ParsedConstantParent
      Value: ParsedBlob } // TODO: Make ParsedConstantBlob type.

[<IsReadOnly; Struct>]
type ConstantParser (sizes: HeapSizes, counts: MetadataTableCounts) =
    member inline private _.Parent = CodedIndex.hasConstant counts
    interface IByteParser<ParsedConstant> with
        member this.Parse buffer =
            if buffer.[1] <> 0uy then failwith "TODO: Handle invalid padding byte error more elegantly."
            let parent = this.Parent
            { Type = LanguagePrimitives.EnumOfValue buffer.[0]
              Parent = { HasConstant = parent.Parse(2, buffer) }
              Value = parse (2 + parent.Length) buffer (BlobParser sizes) }
        member this.Length = 2 + this.Parent.Length + sizes.BlobSize

type [<IsReadOnly; Struct>] ParsedAttributeParent = private { HasCustomAttribute: ParsedCodedIndex }
type [<IsReadOnly; Struct>] ParsedAttributeType = private { CustomAttributeType: ParsedCodedIndex }

[<IsReadOnly; Struct>]
type ParsedCustomAttribute =
    { Parent: ParsedAttributeParent
      Type: ParsedAttributeType
      Value: ParsedAttributeSig }

[<IsReadOnly; Struct>]
type CustomAttributeParser (sizes: HeapSizes, counts: MetadataTableCounts) =
    member inline private _.Parent = CodedIndex.hasCustomAttribute counts
    member inline private _.Type = CodedIndex.customAttributeType counts
    interface IByteParser<ParsedCustomAttribute> with
        member this.Parse buffer =
            let parent = this.Parent
            let ctor = this.Type
            { Parent = { HasCustomAttribute = parent.Parse buffer }
              Type = { CustomAttributeType = ctor.Parse(parent.Length, buffer) }
              Value = { CustomAttrib = parse (parent.Length + ctor.Length) buffer (BlobParser sizes) } }
        member this.Length = this.Parent.Length + this.Type.Length + sizes.BlobSize



[<IsReadOnly; Struct>]
type ParsedClassLayout =
    { PackingSize: uint16
      ClassSize: uint32
      Parent: uint32 }

[<IsReadOnly; Struct>]
type ClassLayoutParser (counts: MetadataTableCounts) =
    member inline private _.Parent = IndexParser MetadataTableFlags.TypeDef
    interface IByteParser<ParsedClassLayout> with
        member this.Parse buffer =
            { PackingSize = Bytes.readU2 0 buffer
              ClassSize = Bytes.readU4 2 buffer
              Parent = this.Parent.Parse(counts, 6, buffer) }
        member this.Length = 6 + this.Parent.Length counts



[<IsReadOnly; Struct>]
type StandaloneSigParser (sizes: HeapSizes) =
    interface IByteParser<ParsedStandaloneSig> with
        member _.Parse buffer = { StandaloneSig = parse 0 buffer (BlobParser sizes) }
        member _.Length = sizes.BlobSize



type [<IsReadOnly; Struct>] ParsedPropertyMap = { Parent: uint32; PropertyList: uint32 }

[<IsReadOnly; Struct>]
type PropertyMapParser (counts: MetadataTableCounts) =
    member inline private _.Parent = IndexParser MetadataTableFlags.TypeDef
    member inline private _.PropertyList = IndexParser MetadataTableFlags.Property
    interface IByteParser<ParsedPropertyMap> with
        member this.Parse buffer =
            let parent = this.Parent
            { Parent = parent.Parse(counts, 0, buffer)
              PropertyList = this.PropertyList.Parse(counts, parent.Length counts, buffer) }
        member this.Length = this.Parent.Length counts + this.PropertyList.Length counts

[<IsReadOnly; Struct>]
type ParsedProperty =
    { Flags: PropertyAttributes
      Name: ParsedString
      Type: ParsedPropertySig }

[<IsReadOnly; Struct>]
type PropertyParser (sizes: HeapSizes) =
    interface IByteParser<ParsedProperty> with
        member _.Parse buffer =
            { Flags = LanguagePrimitives.EnumOfValue(int32(Bytes.readU2 0 buffer))
              Name = parse 2 buffer (StringParser sizes)
              Type = { PropertySig = parse (2 + sizes.StringSize) buffer (BlobParser sizes) } }
        member _.Length = 2 + sizes.StringSize + sizes.BlobSize

type [<IsReadOnly; Struct>] ParsedPropertyAssociation = private { HasSemantics: ParsedCodedIndex }

[<IsReadOnly; Struct>]
type ParsedMethodSemantics =
    { Semantics: MethodSemanticsFlags
      Method: uint32
      Association: ParsedPropertyAssociation }

[<IsReadOnly; Struct>]
type MethodSemanticsParser (counts: MetadataTableCounts) =
    member inline private _.Method = IndexParser MetadataTableFlags.MethodDef
    member inline private _.Association = CodedIndex.hasSemantics counts
    interface IByteParser<ParsedMethodSemantics> with
        member this.Parse buffer =
            let method = this.Method
            { Semantics = LanguagePrimitives.EnumOfValue(Bytes.readU2 0 buffer)
              Method = method.Parse(counts, 2, buffer)
              Association = { HasSemantics = this.Association.Parse(2 + method.Length counts, buffer) } }
        member this.Length = 2 + this.Method.Length counts + this.Association.Length

type [<IsReadOnly; Struct>] ParsedMethodDefOrRef = private { MethodDefOrRef: ParsedCodedIndex }

[<IsReadOnly; Struct>]
type ParsedMethodImpl =
    { Class: uint32
      MethodBody: ParsedMethodDefOrRef
      MethodDeclaration: ParsedMethodDefOrRef }

[<IsReadOnly; Struct>]
type MethodImplParser (counts: MetadataTableCounts) =
    member inline private _.Class = IndexParser MetadataTableFlags.TypeDef
    member inline private _.Method = CodedIndex.methodDefOrRef counts
    interface IByteParser<ParsedMethodImpl> with
        member this.Parse buffer =
            let owner = this.Class
            let method = this.Method
            let moffset = owner.Length counts
            { Class = owner.Parse(counts, 0, buffer)
              MethodBody = { MethodDefOrRef = method.Parse(moffset, buffer) }
              MethodDeclaration = { MethodDefOrRef = method.Parse(moffset + method.Length, buffer) } }
        member this.Length = this.Class.Length counts + (2 * this.Method.Length)



[<IsReadOnly; Struct>]
type TypeSpecParser (sizes: HeapSizes) =
    interface IByteParser<ParsedTypeSpec> with
        member _.Parse buffer = { TypeSpec = parse 0 buffer (BlobParser sizes) }
        member _.Length = sizes.BlobSize



type [<IsReadOnly; Struct>] ParsedFieldRva = { Rva: uint32; Field: uint32 }

[<IsReadOnly; Struct>]
type FieldRvaParser (counts: MetadataTableCounts) =
    member inline private _.Field = IndexParser MetadataTableFlags.Field
    interface IByteParser<ParsedFieldRva> with
        member this.Parse buffer =
            { Rva = Bytes.readU4 0 buffer
              Field = this.Field.Parse(counts, 4, buffer) }
        member this.Length = 4 + this.Field.Length counts

[<IsReadOnly; Struct>]
type ParsedAssembly =
    { HashAlgId: AssemblyHashAlgorithm
      MajorVersion: uint16
      MinorVersion: uint16
      BuildNumber: uint16
      RevisionNumber: uint16
      Flags: AssemblyNameFlags
      PublicKey: ParsedBlob
      Name: ParsedString }

    member this.Version =
        Version(int32 this.MajorVersion, int32 this.MinorVersion, int32 this.BuildNumber, int32 this.RevisionNumber)

[<IsReadOnly; Struct>]
type AssemblyParser (sizes: HeapSizes) =
    interface IByteParser<ParsedAssembly> with
        member _.Parse buffer =
            { HashAlgId = LanguagePrimitives.EnumOfValue(int32(Bytes.readU4 0 buffer))
              MajorVersion = Bytes.readU2 4 buffer
              MinorVersion = Bytes.readU2 6 buffer
              BuildNumber = Bytes.readU2 8 buffer
              RevisionNumber = Bytes.readU2 10 buffer
              Flags = LanguagePrimitives.EnumOfValue(int32(Bytes.readU4 12 buffer))
              PublicKey = parse 16 buffer (BlobParser sizes)
              Name = parse (16 + sizes.BlobSize) buffer (StringParser sizes) }
        member _.Length = 16 + sizes.BlobSize + sizes.StringSize

[<IsReadOnly; Struct>]
type ParsedAssemblyRef =
    { MajorVersion: uint16
      MinorVersion: uint16
      BuildNumber: uint16
      RevisionNumber: uint16
      Flags: AssemblyNameFlags
      PublicKeyOrToken: ParsedBlob
      Name: ParsedString
      Culture: ParsedString
      HashValue: ParsedBlob }

    member this.Version =
        Version(int32 this.MajorVersion, int32 this.MinorVersion, int32 this.BuildNumber, int32 this.RevisionNumber)

[<IsReadOnly; Struct>]
type AssemblyRefParser (sizes: HeapSizes) =
    interface IByteParser<ParsedAssemblyRef> with
        member _.Parse buffer =
            let blob = BlobParser sizes
            let str = StringParser sizes
            { MajorVersion = Bytes.readU2 0 buffer
              MinorVersion = Bytes.readU2 2 buffer
              BuildNumber = Bytes.readU2 4 buffer
              RevisionNumber = Bytes.readU2 6 buffer
              Flags = LanguagePrimitives.EnumOfValue(int32(Bytes.readU4 8 buffer))
              PublicKeyOrToken = parse 12 buffer blob
              Name = parse (12 + sizes.BlobSize) buffer str
              Culture = parse (12 + sizes.BlobSize + sizes.StringSize) buffer str
              HashValue = parse (12 + sizes.BlobSize + (2 * sizes.StringSize)) buffer blob }
        member _.Length = 8 + (2 * sizes.StringSize) + (2 * sizes.BlobSize)



type [<IsReadOnly; Struct>] ParsedImplementation = private { Implementation: ParsedCodedIndex }

[<IsReadOnly; Struct>]
type ParsedManifestResource =
    { Offset: uint32
      Flags: ManifestResourceFlags
      Name: ParsedString
      Implementation: ParsedImplementation }

[<IsReadOnly; Struct>]
type ManifestResourceParser (sizes: HeapSizes, counts: MetadataTableCounts) =
    member inline private _.Implementation = CodedIndex.implementation counts
    interface IByteParser<ParsedManifestResource> with
        member this.Parse buffer =
            let implementation = this.Implementation
            { Offset = Bytes.readU4 0 buffer
              Flags = LanguagePrimitives.EnumOfValue(Bytes.readU4 4 buffer)
              Name = parse 8 buffer (StringParser sizes)
              Implementation = { Implementation = implementation.Parse(8 + sizes.StringSize, buffer) } }
        member this.Length = 8 + sizes.StringSize + this.Implementation.Length

type [<IsReadOnly; Struct>] ParsedNestedClass = { NestedClass: uint32; EnclosingClass: uint32 }

[<IsReadOnly; Struct>]
type NestedClassParser (counts: MetadataTableCounts) =
    member inline private _.Class = IndexParser MetadataTableFlags.TypeDef
    interface IByteParser<ParsedNestedClass> with
        member this.Parse buffer =
            let t = this.Class
            { NestedClass = t.Parse(counts, 0, buffer); EnclosingClass = t.Parse(counts, t.Length counts, buffer) }
        member this.Length = 2 * this.Class.Length counts

type [<IsReadOnly; Struct>] ParsedTypeOrMethodDef = private { TypeOrMethodDef: ParsedCodedIndex }

[<IsReadOnly; Struct>]
type ParsedGenericParam =
    { Number: uint16
      Flags: GenericParameterAttributes
      Owner: ParsedTypeOrMethodDef
      Name: ParsedString }

[<IsReadOnly; Struct>]
type GenericParamParser (sizes: HeapSizes, counts: MetadataTableCounts) =
    member inline private _.Owner = CodedIndex.typeOrMethodDef counts
    interface IByteParser<ParsedGenericParam> with
        member this.Parse buffer =
            let owner = this.Owner
            { Number = Bytes.readU2 0 buffer
              Flags = LanguagePrimitives.EnumOfValue(int32(Bytes.readU2 2 buffer))
              Owner = { TypeOrMethodDef = owner.Parse(4, buffer) }
              Name = parse (4 + owner.Length) buffer (StringParser sizes) }
        member this.Length = 4 + this.Owner.Length + sizes.StringSize

type [<IsReadOnly; Struct>] ParsedMethodSpec = { Method: uint32; Instantiation: ParsedMethodInstantiation }

[<IsReadOnly; Struct>]
type MethodSpecParser (sizes: HeapSizes, counts: MetadataTableCounts) =
    member inline private _.Method = IndexParser MetadataTableFlags.MethodDef
    interface IByteParser<ParsedMethodSpec> with
        member this.Parse buffer =
            let method = this.Method
            { Method = method.Parse(counts, 0, buffer)
              Instantiation = { MethodSpec = parse (method.Length counts) buffer (BlobParser sizes) } }
        member this.Length = this.Method.Length counts + sizes.BlobSize

[<IsReadOnly; Struct>]
type ParsedGenericParamConstraint =
    { Owner: uint32
      Constraint: ParsedTypeDefOrRefOrSpec }

[<IsReadOnly; Struct>]
type GenericParamConstraintParser (counts: MetadataTableCounts) =
    member inline private _.Owner = IndexParser MetadataTableFlags.TypeDef
    member inline private _.Constraint = CodedIndex.typeDefOrRefOrSpec counts
    interface IByteParser<ParsedGenericParamConstraint> with
        member this.Parse buffer =
            let owner = this.Owner
            let { Tag = tag; Index = i } = this.Constraint.Parse(owner.Length counts, buffer)
            { Owner = owner.Parse(counts, 0, buffer)
              Constraint = { Tag = LanguagePrimitives.EnumOfValue tag; TypeIndex = i } }
        member this.Length = this.Owner.Length counts + this.Constraint.Length

(*
[<IsReadOnly; Struct>]
type TemporarySomethingParser (sizes: HeapSizes, counts: MetadataTableCounts) =
    interface IByteParser<Object> with
        member _.Parse buffer =
            failwith "parsing not implemented"
        member _.Length = 0
*)

[<NoComparison; ReferenceEquality>]
type ParsedMetadataTable<'Parser, 'Row when 'Parser :> IByteParser<'Row>> =
    internal
        { Chunk: ChunkReader
          Table: MetadataTableFlags
          TableOffset: uint64
          TableParser: 'Parser
          TableCount: uint32 }

    member this.RowCount = this.TableCount
    /// The size of this metadata table in bytes.
    member this.Size = uint64 this.TableCount * uint64 this.TableParser.Length

    // TODO: Consider having reader functions for each table instead, for better error handling and reporting that includes offset and other information
    member this.TryGetRow(i: uint32) =
        if i >= this.RowCount then Error(MetadataRowOutOfBounds(this.Table, i, this.TableCount))
        else
            let buffer = Span.stackalloc<Byte> this.TableParser.Length
            if this.Chunk.TryReadBytes(this.TableOffset + (uint64 i * uint64 this.TableParser.Length), buffer)
            then Ok(this.TableParser.Parse buffer)
            else Error(StructureOutsideOfCurrentSection(ParsedStructure.MetadataRow(this.Table, i)))

    /// <exception cref="System.ArgumentOutOfRangeException">
    /// Thrown when the index is negative or the table does not contain enough rows.
    /// </exception>
    member this.Item with get(i: int32) =
        if i < 0 then raise(IndexOutOfRangeException())
        else
            match this.TryGetRow(uint32 i) with
            | Error(MetadataRowOutOfBounds _) -> raise(IndexOutOfRangeException())
            | Error err -> failwithf "Error occured while retrieving row at index %i, %O" i err
            | Ok row -> row

type ParsedTypeDefTable = ParsedMetadataTable<TypeDefParser, ParsedTypeDefRow>
type ParsedFieldTable = ParsedMetadataTable<FieldParser, ParsedFieldRow>
type ParsedMethodDefTable = ParsedMetadataTable<MethodDefParser, ParsedMethodRow>
type ParsedInterfaceImplTable = ParsedMetadataTable<InterfaceImplParser, ParsedInterfaceImpl>
type ParsedMemberRefTable = ParsedMetadataTable<MemberRefParser, ParsedMemberRef>
type ParsedConstantTable = ParsedMetadataTable<ConstantParser, ParsedConstant>
type CustomAttributeTable = ParsedMetadataTable<CustomAttributeParser, ParsedCustomAttribute>

type ClassLayoutTable = ParsedMetadataTable<ClassLayoutParser, ParsedClassLayout>

type StandaloneSigTable = ParsedMetadataTable<StandaloneSigParser, ParsedStandaloneSig>

type PropertyMapTable = ParsedMetadataTable<PropertyMapParser, ParsedPropertyMap>
type PropertyTable = ParsedMetadataTable<PropertyParser, ParsedProperty>
type MethodSemanticsTable = ParsedMetadataTable<MethodSemanticsParser, ParsedMethodSemantics>
type MethodImplTable = ParsedMetadataTable<MethodImplParser, ParsedMethodImpl>
type TypeSpecTable = ParsedMetadataTable<TypeSpecParser, ParsedTypeSpec>

type FieldRvaTable = ParsedMetadataTable<FieldRvaParser, ParsedFieldRva>
type AssemblyTable = ParsedMetadataTable<AssemblyParser, ParsedAssembly>
type AssemblyRefTable = ParsedMetadataTable<AssemblyRefParser, ParsedAssemblyRef>

type ManifestResourceTable = ParsedMetadataTable<ManifestResourceParser, ParsedManifestResource>
type NestedClassTable = ParsedMetadataTable<NestedClassParser, ParsedNestedClass>
type GenericParamTable = ParsedMetadataTable<GenericParamParser, ParsedGenericParam>
type MethodSpecTable = ParsedMetadataTable<MethodSpecParser, ParsedMethodSpec>
type GenericParamConstraintTable = ParsedMetadataTable<GenericParamConstraintParser, ParsedGenericParamConstraint>
//type TemporarySomethingTable = ParsedMetadataTable<unit, unit>

[<NoComparison; ReferenceEquality>]
type ParsedMetadataTables =
    private
        { Chunk: ChunkReader
          TablesHeader: ParsedMetadataTablesHeader
          TablesOffset: uint64
          [<DefaultValue>] mutable TablesSize: uint64
          [<DefaultValue>] mutable ModuleTable: ParsedMetadataTable<ModuleParser, ParsedModuleRow>
          [<DefaultValue>] mutable TypeRefTable: ParsedMetadataTable<TypeRefParser, ParsedTypeRefRow> voption
          [<DefaultValue>] mutable TypeDefTable: ParsedTypeDefTable voption
          [<DefaultValue>] mutable FieldTable: ParsedFieldTable voption
          [<DefaultValue>] mutable MethodDefTable: ParsedMethodDefTable voption
          [<DefaultValue>] mutable ParamTable: ParsedMetadataTable<ParamParser, ParsedParamRow> voption
          [<DefaultValue>] mutable InterfaceImplTable: ParsedInterfaceImplTable voption
          [<DefaultValue>] mutable MemberRefTable: ParsedMemberRefTable voption
          [<DefaultValue>] mutable ConstantTable: ParsedConstantTable voption
          [<DefaultValue>] mutable CustomAttributeTable: CustomAttributeTable voption

          [<DefaultValue>] mutable ClassLayoutTable: ClassLayoutTable voption

          [<DefaultValue>] mutable StandaloneSigTable: StandaloneSigTable voption

          [<DefaultValue>] mutable PropertyMapTable: PropertyMapTable voption
          [<DefaultValue>] mutable PropertyTable: PropertyTable voption
          [<DefaultValue>] mutable MethodSemanticsTable: MethodSemanticsTable voption
          [<DefaultValue>] mutable MethodImplTable: MethodImplTable voption
          [<DefaultValue>] mutable TypeSpecTable: TypeSpecTable voption

          [<DefaultValue>] mutable FieldRvaTable: FieldRvaTable voption
          [<DefaultValue>] mutable AssemblyTable: AssemblyTable voption
          [<DefaultValue>] mutable AssemblyRefTable: AssemblyRefTable voption

          [<DefaultValue>] mutable ManifestResourceTable: ManifestResourceTable voption
          [<DefaultValue>] mutable NestedClassTable: NestedClassTable voption
          [<DefaultValue>] mutable GenericParamTable: GenericParamTable voption
          [<DefaultValue>] mutable MethodSpecTable: MethodSpecTable voption
          [<DefaultValue>] mutable GenericParamConstraintTable: GenericParamConstraintTable voption
          //[<DefaultValue>] mutable TemporarySomethingTable: unit voption
          }

    member this.Header = this.TablesHeader
    /// The size of the metadata tables in bytes.
    member this.Size = this.TablesSize
    /// Offset from start of section containing the CLI metadata to the first byte of the first row of the first table.
    member this.Offset = this.TablesOffset

    member this.Module =
        if this.ModuleTable = Unchecked.defaultof<_> then invalidOp "Unable to find module table"
        this.ModuleTable
    member this.TypeRef = this.TypeRefTable
    member this.TypeDef = this.TypeDefTable
    member this.Field = this.FieldTable
    member this.MethodDef = this.MethodDefTable
    member this.Param = this.ParamTable
    member this.InterfaceImpl = this.InterfaceImplTable
    member this.MemberRef = this.MemberRefTable
    member this.Constant = this.ConstantTable
    member this.CustomAttribute = this.CustomAttributeTable

    member this.ClassLayout = this.ClassLayoutTable

    member this.StandaloneSig = this.StandaloneSigTable

    member this.PropertyMap = this.PropertyMapTable
    member this.Property = this.PropertyTable
    member this.MethodSemantics = this.MethodSemanticsTable
    member this.MethodImpl = this.MethodImplTable
    member this.TypeSpec = this.TypeSpecTable

    member this.FieldRva = this.FieldRvaTable
    member this.Assembly = this.AssemblyTable
    member this.AssemblyRef = this.AssemblyRefTable

    member this.ManifestResource = this.ManifestResourceTable
    member this.NestedClass = this.NestedClassTable
    member this.GenericParam = this.GenericParamTable
    member this.MethodSpec = this.MethodSpecTable
    member this.GenericParamConstraint = this.GenericParamConstraintTable

[<RequireQualifiedAccess>]
module ParsedMetadataTables =
    let internal create chunk header offset =
        let tables =
            { Chunk = chunk
              TablesHeader = header
              TablesOffset = offset }
        for KeyValue(table, count) in header.Rows do // TODO: How to ensure that keys are in order?
            let inline createTable parser =
                { Chunk = chunk
                  Table = table
                  TableOffset = tables.TablesSize + offset
                  TableParser = parser
                  TableCount = count }
            let inline createOptionalTable parser = ValueSome(createTable parser)
            match table with
            | MetadataTableFlags.Module ->
                tables.ModuleTable <- createTable(ModuleParser header.HeapSizes)
                tables.TablesSize <- tables.TablesSize + tables.ModuleTable.Size
            | MetadataTableFlags.TypeRef ->
                tables.TypeRefTable <- TypeRefParser(header.HeapSizes, header.Rows) |> createOptionalTable
                tables.TablesSize <- tables.TablesSize + tables.TypeRefTable.Value.Size
            | MetadataTableFlags.TypeDef ->
                tables.TypeDefTable <- TypeDefParser(header.HeapSizes, header.Rows) |> createOptionalTable
                tables.TablesSize <- tables.TablesSize + tables.TypeDefTable.Value.Size
            | MetadataTableFlags.Field ->
                tables.FieldTable <- createOptionalTable(FieldParser header.HeapSizes)
                tables.TablesSize <- tables.TablesSize + tables.FieldTable.Value.Size
            | MetadataTableFlags.MethodDef ->
                tables.MethodDefTable <- MethodDefParser(header.HeapSizes, header.Rows) |> createOptionalTable
                tables.TablesSize <- tables.TablesSize + tables.MethodDefTable.Value.Size
            | MetadataTableFlags.Param ->
                tables.ParamTable <- createOptionalTable(ParamParser header.HeapSizes)
                tables.TablesSize <- tables.TablesSize + tables.ParamTable.Value.Size
            | MetadataTableFlags.InterfaceImpl ->
                tables.InterfaceImplTable <- createOptionalTable(InterfaceImplParser header.Rows)
                tables.TablesSize <- tables.TablesSize + tables.InterfaceImplTable.Value.Size
            | MetadataTableFlags.MemberRef ->
                tables.MemberRefTable <- MemberRefParser(header.HeapSizes, header.Rows) |> createOptionalTable
                tables.TablesSize <- tables.TablesSize + tables.MemberRefTable.Value.Size
            | MetadataTableFlags.Constant ->
                tables.ConstantTable <- ConstantParser(header.HeapSizes, header.Rows) |> createOptionalTable
                tables.TablesSize <- tables.TablesSize + tables.ConstantTable.Value.Size
            | MetadataTableFlags.CustomAttribute ->
                tables.CustomAttributeTable <- CustomAttributeParser(header.HeapSizes, header.Rows) |> createOptionalTable
                tables.TablesSize <- tables.TablesSize + tables.CustomAttributeTable.Value.Size

            | MetadataTableFlags.ClassLayout ->
                tables.ClassLayoutTable <- createOptionalTable (ClassLayoutParser header.Rows)
                tables.TablesSize <- tables.TablesSize + tables.ClassLayoutTable.Value.Size

            | MetadataTableFlags.StandAloneSig ->
                tables.StandaloneSigTable <- createOptionalTable(StandaloneSigParser header.HeapSizes)
                tables.TablesSize <- tables.TablesSize + tables.StandaloneSigTable.Value.Size

            | MetadataTableFlags.PropertyMap ->
                tables.PropertyMapTable <- createOptionalTable(PropertyMapParser header.Rows)
                tables.TablesSize <- tables.TablesSize + tables.PropertyMapTable.Value.Size
            | MetadataTableFlags.Property ->
                tables.PropertyTable <- createOptionalTable(PropertyParser header.HeapSizes)
                tables.TablesSize <- tables.TablesSize + tables.PropertyTable.Value.Size
            | MetadataTableFlags.MethodSemantics ->
                tables.MethodSemanticsTable <- createOptionalTable(MethodSemanticsParser header.Rows)
                tables.TablesSize <- tables.TablesSize + tables.MethodSemanticsTable.Value.Size
            | MetadataTableFlags.MethodImpl ->
                tables.MethodImplTable <- createOptionalTable(MethodImplParser header.Rows)
                tables.TablesSize <- tables.TablesSize + tables.MethodImplTable.Value.Size

            | MetadataTableFlags.TypeSpec ->
                tables.TypeSpecTable <- createOptionalTable(TypeSpecParser header.HeapSizes)
                tables.TablesSize <- tables.TablesSize + tables.TypeSpecTable.Value.Size

            | MetadataTableFlags.FieldRva ->
                tables.FieldRvaTable <- createOptionalTable(FieldRvaParser header.Rows)
                tables.TablesSize <- tables.TablesSize + tables.FieldRvaTable.Value.Size
            | MetadataTableFlags.Assembly ->
                tables.AssemblyTable <- createOptionalTable(AssemblyParser header.HeapSizes)
                tables.TablesSize <- tables.TablesSize + tables.AssemblyTable.Value.Size
            | MetadataTableFlags.AssemblyRef ->
                tables.AssemblyRefTable <- createOptionalTable(AssemblyRefParser header.HeapSizes)
                tables.TablesSize <- tables.TablesSize + tables.AssemblyRefTable.Value.Size

            | MetadataTableFlags.ManifestResource ->
                tables.ManifestResourceTable <- ManifestResourceParser(header.HeapSizes, header.Rows) |> createOptionalTable
                tables.TablesSize <- tables.TablesSize + tables.ManifestResourceTable.Value.Size
            | MetadataTableFlags.NestedClass ->
                tables.NestedClassTable <- createOptionalTable(NestedClassParser header.Rows)
                tables.TablesSize <- tables.TablesSize + tables.NestedClassTable.Value.Size
            | MetadataTableFlags.GenericParam ->
                tables.GenericParamTable <- GenericParamParser(header.HeapSizes, header.Rows) |> createOptionalTable
                tables.TablesSize <- tables.TablesSize + tables.GenericParamTable.Value.Size
            | MetadataTableFlags.MethodSpec ->
                tables.MethodSpecTable <- MethodSpecParser(header.HeapSizes, header.Rows) |> createOptionalTable
                tables.TablesSize <- tables.TablesSize + tables.MethodSpecTable.Value.Size
            | MetadataTableFlags.GenericParamConstraint ->
                tables.GenericParamConstraintTable <- createOptionalTable(GenericParamConstraintParser header.Rows)
                tables.TablesSize <- tables.TablesSize + tables.GenericParamConstraintTable.Value.Size
            | _ -> failwithf "Table %A is currently not supported" table
        tables
