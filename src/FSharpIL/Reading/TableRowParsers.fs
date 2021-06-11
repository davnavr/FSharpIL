namespace FSharpIL.Reading

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<IsReadOnly; Struct>]
type internal TableIndexParser<'Row when 'Row :> ITableRow> (table: ValidTableFlags, counts: ITableRowCounts) =
    interface IByteParser<TableIndex<'Row>> with
        member _.Length = if counts.RowCount table <= 0xFFFFu then 2u else 4u
        member _.Parse buffer =
            { TableIndex =
                match buffer.Length with
                | 2u -> uint32(ChunkedMemory.readU2 0u &buffer)
                | 4u
                | _ -> uint32(ChunkedMemory.readU4 0u &buffer) }

[<RequireQualifiedAccess>]
module CodedIndex =
    let inline internal createCodedIndex (index: 'T) cast (kind: inref<CodedIndexKind<'Tag>>) =
        let n = kind.NumEncodingBits
        let filter = cast kind.MaxSmallIndex <<< n
        CodedIndex<'Tag>(LanguagePrimitives.EnumOfValue(uint8(index &&& ~~~filter)), uint32((index &&& filter) >>> n))

    [<IsReadOnly; Struct>]
    type Parser<'Tag when 'Tag : enum<uint8>> = struct
        /// Gets a value indicating whether this coded index would occupy four bytes.
        val IsLarge: bool
        val Kind: CodedIndexKind<'Tag>

        internal new (kind: inref<CodedIndexKind<'Tag>>, counts) =
            { Kind = kind
              IsLarge = kind.IsLarge counts }

        interface IByteParser<CodedIndex<'Tag>> with
            /// The number of bytes that this coded index would occupy.
            member this.Length = if this.IsLarge then 4u else 2u
            member this.Parse buffer =
                if this.IsLarge
                then createCodedIndex (ChunkedMemory.readU4 0u &buffer) uint32 &this.Kind
                else createCodedIndex (ChunkedMemory.readU2 0u &buffer) uint16 &this.Kind
    end
    
    let typeDefOrRef counts = Parser(&CodedIndexKinds.TypeDefOrRef, counts)
    let hasConstant counts = Parser(&CodedIndexKinds.HasConstant, counts)
    let hasCustomAttribute counts = Parser(&CodedIndexKinds.HasCustomAttribute, counts)
    let hasFieldMarshal counts = Parser(&CodedIndexKinds.HasFieldMarshal, counts)
    let hasDeclSecurity counts = Parser(&CodedIndexKinds.HasDeclSecurity, counts)
    let memberRefParent counts = Parser(&CodedIndexKinds.MemberRefParent, counts)
    let hasSemantics counts = Parser(&CodedIndexKinds.HasSemantics, counts)
    let methodDefOrRef counts = Parser(&CodedIndexKinds.MethodDefOrRef, counts)
    let memberForwarded counts = Parser(&CodedIndexKinds.MemberForwarded, counts)
    let implementation counts = Parser(&CodedIndexKinds.Implementation, counts)
    let customAttributeType counts = Parser(&CodedIndexKinds.CustomAttributeType, counts)
    let resolutionScopeParser counts = Parser(&CodedIndexKinds.ResolutionScope, counts)
    let typeOrMethodDef counts = Parser(&CodedIndexKinds.TypeOrMethodDef, counts)

[<RequireQualifiedAccess>]
module internal Offset =
    let parse (buffer: ChunkedMemory) =
        match buffer.Length with
        | 4u -> ChunkedMemory.readU4 0u &buffer
        | 2u -> uint32(ChunkedMemory.readU2 0u &buffer)
        | bad -> invalidArg "buffer" (sprintf "Invalid buffer length %i, expected length of 2 or 4" bad)

[<IsReadOnly; Struct>]
type internal StringParser (sizes: HeapSizes) =
    interface IByteParser<StringOffset> with
        member _.Parse buffer = { StringOffset = Offset.parse buffer }
        member _.Length = sizes.StringSize

[<IsReadOnly; Struct>]
type internal GuidParser (sizes: HeapSizes) =
    interface IByteParser<GuidIndex> with
        member _.Parse buffer = { GuidIndex = Offset.parse buffer }
        member _.Length = sizes.GuidSize

[<IsReadOnly; Struct>]
type internal BlobParser (sizes: HeapSizes) =
    interface IByteParser<BlobOffset> with
        member _.Parse buffer = { BlobOffset = Offset.parse buffer }
        member _.Length = sizes.BlobSize

[<IsReadOnly; Struct>]
type ModuleParser (sizes: HeapSizes) =
    interface IByteParser<ModuleRow> with
        member _.Parse buffer =
            let guid = GuidParser sizes
            let goffset = 2u + sizes.StringSize
            { Generation = ChunkedMemory.readU2 0u &buffer
              Name = ByteParser.parse 2u &buffer (StringParser sizes)
              Mvid = ByteParser.parse goffset &buffer guid
              EncId = ByteParser.parse (goffset + sizes.GuidSize) &buffer guid
              EncBaseId = ByteParser.parse (goffset + (2u * sizes.GuidSize)) &buffer guid }
        member _.Length = 2u + sizes.StringSize + (3u * sizes.GuidSize)

[<IsReadOnly; Struct>]
type TypeRefParser (sizes: HeapSizes, counts: ITableRowCounts) =
    member inline private _.ResolutionScope = CodedIndex.resolutionScopeParser counts
    interface IByteParser<TypeRefRow> with
        member this.Parse buffer =
            let str = StringParser sizes
            let rscope = this.ResolutionScope
            { ResolutionScope = ByteParser.parse 0u &buffer rscope
              TypeName = ByteParser.parse (ByteParser.length rscope) &buffer str
              TypeNamespace = ByteParser.parse (ByteParser.length rscope + sizes.StringSize) &buffer str }
        member this.Length = ByteParser.length this.ResolutionScope + (2u * sizes.StringSize)

[<IsReadOnly; Struct>]
type TypeDefParser (sizes: HeapSizes, counts: ITableRowCounts) =
    member inline private _.Extends = CodedIndex.typeDefOrRef counts
    member inline private _.FieldList = TableIndexParser(ValidTableFlags.Field, counts)
    member inline private _.MethodList = TableIndexParser(ValidTableFlags.MethodDef, counts)
    interface IByteParser<TypeDefRow> with
        member this.Parse buffer =
            let str = StringParser sizes
            let extends = this.Extends
            let eoffset = 4u + (2u * sizes.StringSize)
            let field = this.FieldList
            { Flags = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU4 0u &buffer)
              TypeName = ByteParser.parse 4u &buffer str
              TypeNamespace = ByteParser.parse (4u + sizes.StringSize) &buffer str
              Extends = ByteParser.parse eoffset &buffer extends
              FieldList = ByteParser.parse (eoffset + ByteParser.length extends) &buffer field
              MethodList =
                ByteParser.parse
                    (eoffset + ByteParser.length extends + ByteParser.length field)
                    &buffer
                    this.MethodList }
        member this.Length =
            4u
            + (2u * sizes.StringSize)
            + ByteParser.length this.Extends
            + (ByteParser.length this.FieldList)
            + (ByteParser.length this.MethodList)

[<IsReadOnly; Struct>]
type FieldParser (sizes: HeapSizes) =
    interface IByteParser<FieldRow> with
        member _.Parse buffer =
            { Flags = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU2 0u &buffer)
              Name = ByteParser.parse 2u &buffer (StringParser sizes)
              Signature = { FieldSig = ByteParser.parse (2u + sizes.StringSize) &buffer (BlobParser sizes) } }
        member _.Length = 2u + sizes.StringSize + sizes.BlobSize

[<IsReadOnly; Struct>]
type MethodDefParser (sizes: HeapSizes, counts: ITableRowCounts) =
    member inline private _.ParamList = TableIndexParser(ValidTableFlags.Param, counts)
    interface IByteParser<MethodDefRow> with
        member this.Parse buffer =
            { Rva = Rva(ChunkedMemory.readU4 0u &buffer)
              ImplFlags = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU2 4u &buffer)
              Flags = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU2 6u &buffer)
              Name = ByteParser.parse 8u &buffer (StringParser sizes)
              Signature = { MethodDefSig = ByteParser.parse (8u + sizes.StringSize) &buffer (BlobParser sizes) }
              ParamList = ByteParser.parse (8u + sizes.StringSize + sizes.BlobSize) &buffer this.ParamList }
        member this.Length = 8u + sizes.StringSize + sizes.BlobSize + (ByteParser.length this.ParamList)

[<IsReadOnly; Struct>]
type ParamParser (sizes: HeapSizes) =
    interface IByteParser<ParamRow> with
        member _.Parse buffer =
            { Flags = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU2 0u &buffer)
              Sequence = ChunkedMemory.readU2 2u &buffer
              Name  = ByteParser.parse 4u &buffer (StringParser sizes) }
        member _.Length = 4u + sizes.StringSize

[<IsReadOnly; Struct>]
type InterfaceImplParser (counts: ITableRowCounts) =
    member inline private _.Class = TableIndexParser(ValidTableFlags.TypeDef, counts)
    member inline private _.Interface = CodedIndex.typeDefOrRef counts
    interface IByteParser<InterfaceImplRow> with
        member this.Parse buffer =
            let parent = this.Class
            { Class = ByteParser.parse 0u &buffer parent
              Interface = ByteParser.parse (ByteParser.length parent) &buffer this.Interface }
        member this.Length = ByteParser.length this.Class + ByteParser.length this.Interface

[<IsReadOnly; Struct>]
type MemberRefParser (sizes: HeapSizes, counts: ITableRowCounts) =
    member inline private _.Class = CodedIndex.memberRefParent counts
    interface IByteParser<MemberRefRow> with
        member this.Parse buffer =
            let parent = this.Class
            { Class = ByteParser.parse 0u &buffer parent
              Name  = ByteParser.parse (ByteParser.length parent) &buffer (StringParser sizes)
              Signature =
                { MemberRefSig = ByteParser.parse (ByteParser.length parent + sizes.StringSize) &buffer (BlobParser sizes) } }
        member this.Length = ByteParser.length this.Class + sizes.StringSize + sizes.BlobSize

[<IsReadOnly; Struct>]
type ConstantParser (sizes: HeapSizes, counts: ITableRowCounts) =
    member inline private _.Parent = CodedIndex.hasConstant counts
    interface IByteParser<ConstantRow> with
        member this.Parse buffer =
            if buffer.[1u] <> 0uy then failwith "TODO: Handle invalid padding byte error more elegantly."
            let parent = this.Parent
            { Type = LanguagePrimitives.EnumOfValue buffer.[0u]
              Parent = ByteParser.parse 2u &buffer parent
              Value = { Constant = ByteParser.parse (2u + ByteParser.length parent) &buffer (BlobParser sizes) } }
        member this.Length = 2u + ByteParser.length this.Parent + sizes.BlobSize

[<IsReadOnly; Struct>]
type CustomAttributeParser (sizes: HeapSizes, counts: ITableRowCounts) =
    member inline private _.Parent = CodedIndex.hasCustomAttribute counts
    member inline private _.Type = CodedIndex.customAttributeType counts
    interface IByteParser<CustomAttributeRow> with
        member this.Parse buffer =
            let parent = this.Parent
            let ctor = this.Type
            { Parent = ByteParser.parse 0u &buffer parent
              Type = ByteParser.parse (ByteParser.length parent) &buffer ctor
              Value =
                { CustomAttrib =
                    ByteParser.parse (ByteParser.length parent + ByteParser.length ctor) &buffer (BlobParser sizes) } }
        member this.Length = ByteParser.length this.Parent + ByteParser.length this.Type + sizes.BlobSize



[<IsReadOnly; Struct>]
type ClassLayoutParser (counts: ITableRowCounts) =
    member inline private _.Parent = TableIndexParser(ValidTableFlags.TypeDef, counts)
    interface IByteParser<ClassLayoutRow> with
        member this.Parse buffer =
            { PackingSize = ChunkedMemory.readU2 0u &buffer
              ClassSize = ChunkedMemory.readU4 2u &buffer
              Parent = ByteParser.parse 6u &buffer this.Parent }
        member this.Length = 6u + ByteParser.length this.Parent



[<IsReadOnly; Struct>]
type StandaloneSigParser (sizes: HeapSizes) =
    interface IByteParser<StandaloneSigRow> with
        member _.Parse buffer = { StandAloneSig = ByteParser.parse 0u &buffer (BlobParser sizes) }
        member _.Length = sizes.BlobSize



[<IsReadOnly; Struct>]
type PropertyMapParser (counts: ITableRowCounts) =
    member inline private _.Parent = TableIndexParser(ValidTableFlags.TypeDef, counts)
    member inline private _.PropertyList = TableIndexParser(ValidTableFlags.Property, counts)
    interface IByteParser<PropertyMapRow> with
        member this.Parse buffer =
            let parent = this.Parent
            { Parent = ByteParser.parse 0u &buffer parent
              PropertyList = ByteParser.parse (ByteParser.length parent) &buffer this.PropertyList }
        member this.Length = ByteParser.length this.Parent + ByteParser.length this.PropertyList

[<IsReadOnly; Struct>]
type PropertyParser (sizes: HeapSizes) =
    interface IByteParser<PropertyRow> with
        member _.Parse buffer =
            { Flags = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU2 0u &buffer)
              Name  = ByteParser.parse 2u &buffer (StringParser sizes)
              Type = { PropertySig = ByteParser.parse (2u + sizes.StringSize) &buffer (BlobParser sizes) } }
        member _.Length = 2u + sizes.StringSize + sizes.BlobSize

[<IsReadOnly; Struct>]
type MethodSemanticsParser (counts: ITableRowCounts) =
    member inline private _.Method = TableIndexParser(ValidTableFlags.MethodDef, counts)
    member inline private _.Association = CodedIndex.hasSemantics counts
    interface IByteParser<MethodSemanticsRow> with
        member this.Parse buffer =
            let method = this.Method
            { Semantics = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU2 0u &buffer)
              Method = ByteParser.parse 2u &buffer method
              Association = ByteParser.parse (2u + ByteParser.length method) &buffer this.Association }
        member this.Length = 2u + ByteParser.length this.Method + ByteParser.length this.Association

[<IsReadOnly; Struct>]
type MethodImplParser (counts: ITableRowCounts) =
    member inline private _.Class = TableIndexParser(ValidTableFlags.TypeDef, counts)
    member inline private _.Method = CodedIndex.methodDefOrRef counts
    interface IByteParser<MethodImplRow> with
        member this.Parse buffer =
            let owner = this.Class
            let method = this.Method
            let moffset = ByteParser.length owner
            { Class = ByteParser.parse 0u &buffer owner
              MethodBody = ByteParser.parse moffset &buffer method
              MethodDeclaration = ByteParser.parse (moffset + ByteParser.length method) &buffer method }
        member this.Length = ByteParser.length this.Class + (2u * ByteParser.length this.Method)



[<IsReadOnly; Struct>]
type TypeSpecParser (sizes: HeapSizes) =
    interface IByteParser<TypeSpecRow> with
        member _.Parse buffer = { TypeSpec = ByteParser.parse 0u &buffer (BlobParser sizes) }
        member _.Length = sizes.BlobSize

[<IsReadOnly; Struct>]
type FieldRvaParser (counts: ITableRowCounts) =
    member inline private _.Field = TableIndexParser(ValidTableFlags.Field, counts)
    interface IByteParser<FieldRvaRow> with
        member this.Parse buffer =
            { Rva = Rva(ChunkedMemory.readU4 0u &buffer)
              Field = ByteParser.parse 4u &buffer this.Field }
        member this.Length = 4u + ByteParser.length this.Field

[<IsReadOnly; Struct>]
type AssemblyParser (sizes: HeapSizes) =
    interface IByteParser<AssemblyRow> with
        member _.Parse buffer =
            { HashAlgId = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU4 0u &buffer)
              MajorVersion = ChunkedMemory.readU2 4u &buffer
              MinorVersion = ChunkedMemory.readU2 6u &buffer
              BuildNumber = ChunkedMemory.readU2 8u &buffer
              RevisionNumber = ChunkedMemory.readU2 10u &buffer
              Flags = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU4 12u &buffer)
              PublicKey  = ByteParser.parse 16u &buffer (BlobParser sizes)
              Name  = ByteParser.parse (16u + sizes.BlobSize) &buffer (StringParser sizes)
              Culture  = ByteParser.parse (16u + sizes.BlobSize + sizes.StringSize) &buffer (StringParser sizes) }
        member _.Length = 16u + sizes.BlobSize + (2u * sizes.StringSize)

[<IsReadOnly; Struct>]
type AssemblyRefParser (sizes: HeapSizes) =
    interface IByteParser<AssemblyRefRow> with
        member _.Parse buffer =
            let blob = BlobParser sizes
            let str = StringParser sizes
            { MajorVersion = ChunkedMemory.readU2 0u &buffer
              MinorVersion = ChunkedMemory.readU2 2u &buffer
              BuildNumber = ChunkedMemory.readU2 4u &buffer
              RevisionNumber = ChunkedMemory.readU2 6u &buffer
              Flags = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU4 8u &buffer)
              PublicKeyOrToken  = ByteParser.parse 12u &buffer blob
              Name  = ByteParser.parse (12u + sizes.BlobSize) &buffer str
              Culture  = ByteParser.parse (12u + sizes.BlobSize + sizes.StringSize) &buffer str
              HashValue  = ByteParser.parse (12u + sizes.BlobSize + (2u * sizes.StringSize)) &buffer blob }
        member _.Length = 12u + (2u * sizes.StringSize) + (2u * sizes.BlobSize)



[<IsReadOnly; Struct>]
type ManifestResourceParser (sizes: HeapSizes, counts: ITableRowCounts) =
    member inline private _.Implementation = CodedIndex.implementation counts
    interface IByteParser<ManifestResourceRow> with
        member this.Parse buffer =
            { Offset = ChunkedMemory.readU4 0u &buffer
              Flags = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU4 4u &buffer)
              Name  = ByteParser.parse 8u &buffer (StringParser sizes)
              Implementation = ByteParser.parse (8u + sizes.StringSize) &buffer this.Implementation }
        member this.Length = 8u + sizes.StringSize + ByteParser.length this.Implementation

[<IsReadOnly; Struct>]
type NestedClassParser (counts: ITableRowCounts) =
    member inline private _.Class = TableIndexParser(ValidTableFlags.TypeDef, counts)
    interface IByteParser<NestedClassRow> with
        member this.Parse buffer =
            let typeDef = this.Class
            { NestedClass = ByteParser.parse 0u &buffer typeDef
              EnclosingClass = ByteParser.parse (ByteParser.length typeDef) &buffer typeDef }
        member this.Length = 2u * ByteParser.length this.Class

[<IsReadOnly; Struct>]
type GenericParamParser (sizes: HeapSizes, counts: ITableRowCounts) =
    member inline private _.Owner = CodedIndex.typeOrMethodDef counts
    interface IByteParser<GenericParamRow> with
        member this.Parse buffer =
            let owner = this.Owner
            { Number = ChunkedMemory.readU2 0u &buffer
              Flags = LanguagePrimitives.EnumOfValue(ChunkedMemory.readU2 2u &buffer)
              Owner = ByteParser.parse 4u &buffer owner
              Name  = ByteParser.parse (4u + ByteParser.length owner) &buffer (StringParser sizes) }
        member this.Length = 4u + ByteParser.length this.Owner + sizes.StringSize

[<IsReadOnly; Struct>]
type MethodSpecParser (sizes: HeapSizes, counts: ITableRowCounts) =
    member inline private _.Method = CodedIndex.methodDefOrRef counts
    interface IByteParser<MethodSpecRow> with
        member this.Parse buffer =
            let method = this.Method
            { Method = ByteParser.parse 0u &buffer method
              Instantiation = { MethodSpec = ByteParser.parse (ByteParser.length method) &buffer (BlobParser sizes) } }
        member this.Length = ByteParser.length this.Method + sizes.BlobSize

[<IsReadOnly; Struct>]
type GenericParamConstraintParser (counts: ITableRowCounts) =
    member inline private _.Owner = TableIndexParser(ValidTableFlags.TypeDef, counts)
    member inline private _.Constraint = CodedIndex.typeDefOrRef counts
    interface IByteParser<GenericParamConstraintRow> with
        member this.Parse buffer =
            let owner = this.Owner
            { Owner = ByteParser.parse 0u &buffer owner
              Constraint = ByteParser.parse (ByteParser.length owner) &buffer this.Constraint }
        member this.Length = ByteParser.length this.Owner + ByteParser.length this.Constraint

(*
[<IsReadOnly; Struct>]
type TemporarySomethingParser (sizes: HeapSizes, counts: ITableRowCounts) =
    interface IByteParser<Object> with
        member _.Parse buffer =
            failwith "parsing not implemented"
        member _.Length = 0
*)
