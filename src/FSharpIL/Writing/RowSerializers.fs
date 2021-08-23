[<RequireQualifiedAccess>]
module FSharpIL.Writing.RowSerializers

open FSharpIL.Metadata.Tables

type ISerializer<'Row when 'Row :> ITableRow and 'Row : struct> = interface
    abstract Serialize:
        HeapSizes *
        ITableRowCounts *
        row: inref<'Row> *
        methodBodiesRva: FSharpIL.PortableExecutable.Rva *
        embeddedDataRva: FSharpIL.PortableExecutable.Rva *
        byref<FSharpIL.ChunkedMemoryBuilder> -> unit
end

type TypeRef = struct
    interface ISerializer<TypeRefRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            WriteIndex.coded &wr tsizes &CodedIndexKinds.ResolutionScope row.ResolutionScope
            WriteIndex.string &wr hsizes row.TypeName.Offset
            WriteIndex.string &wr hsizes row.TypeNamespace
end

type TypeDef = struct
    interface ISerializer<TypeDefRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            wr.WriteLE(uint32 row.Flags)
            WriteIndex.string &wr hsizes row.TypeName.Offset
            WriteIndex.string &wr hsizes row.TypeNamespace
            WriteIndex.coded &wr tsizes &CodedIndexKinds.TypeDefOrRef row.Extends
            WriteIndex.table &wr tsizes ValidTableFlags.Field row.FieldList
            WriteIndex.table &wr tsizes ValidTableFlags.Field row.MethodList
end

type Field = struct
    interface ISerializer<FieldRow> with
        member _.Serialize(hsizes, _, row, _, _, wr) =
            wr.WriteLE(uint16 row.Flags)
            WriteIndex.string &wr hsizes row.Name.Offset
            WriteIndex.blob &wr hsizes row.Signature.FieldSig
end

type MethodDef = struct
    interface ISerializer<MethodDefRow> with
        member _.Serialize(hsizes, tsizes, row, methodBodiesRva, _, wr) =
            let rva: FSharpIL.PortableExecutable.Rva = row.Rva + methodBodiesRva
            wr.WriteLE(uint32 rva)
            wr.WriteLE(uint16 row.ImplFlags)
            wr.WriteLE(uint16 row.Flags)
            WriteIndex.string &wr hsizes row.Name.Offset
            WriteIndex.blob &wr hsizes row.Signature.MethodDefSig
            WriteIndex.table &wr tsizes ValidTableFlags.Param row.ParamList
end

type Param = struct
    interface ISerializer<ParamRow> with
        member _.Serialize(hsizes, _, row, _, _, wr) =
            wr.WriteLE(uint16 row.Flags)
            wr.WriteLE row.Sequence
            WriteIndex.string &wr hsizes row.Name
end

type InterfaceImpl = struct
    interface ISerializer<InterfaceImplRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            WriteIndex.table &wr tsizes ValidTableFlags.TypeDef row.Class
            WriteIndex.coded &wr tsizes &CodedIndexKinds.TypeDefOrRef row.Interface
end

type MemberRef = struct
    interface ISerializer<MemberRefRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            WriteIndex.coded &wr tsizes &CodedIndexKinds.MemberRefParent row.Class
            WriteIndex.string &wr hsizes row.Name.Offset
            WriteIndex.blob &wr hsizes row.Signature.MemberRefSig
end

type Constant = struct
    interface ISerializer<ConstantRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            wr.Write(uint8 row.Type)
            wr.Write 0uy // Padding
            WriteIndex.coded &wr tsizes &CodedIndexKinds.HasConstant row.Parent
            WriteIndex.blob &wr hsizes row.Value.Constant
end

type CustomAttribute = struct
    interface ISerializer<CustomAttributeRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            WriteIndex.coded &wr tsizes &CodedIndexKinds.HasCustomAttribute row.Parent
            WriteIndex.coded &wr tsizes &CodedIndexKinds.CustomAttributeType row.Type
            WriteIndex.blob &wr hsizes row.Value.CustomAttrib
end



[<Struct>]
type ClassLayout =
    interface ISerializer<ClassLayoutRow> with
        member _.Serialize(_, tsizes, row, _, _, wr) =
            wr.WriteLE(uint16 row.PackingSize)
            wr.WriteLE(uint32 row.ClassSize)
            WriteIndex.table &wr tsizes ValidTableFlags.TypeDef row.Parent



type StandAloneSig = struct
    interface ISerializer<StandaloneSigRow> with
        member _.Serialize(hsizes, _, row, _, _, wr) = WriteIndex.blob &wr hsizes row.Signature
end

type EventMap = struct
    interface ISerializer<EventMapRow> with
        member _.Serialize(_, tsizes, row, _, _, wr) =
            WriteIndex.table &wr tsizes ValidTableFlags.TypeDef row.Parent
            WriteIndex.table &wr tsizes ValidTableFlags.Event row.EventList
end

type Event = struct
    interface ISerializer<EventRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            wr.WriteLE(uint16 row.EventFlags)
            WriteIndex.string &wr hsizes row.Name.Offset
            WriteIndex.coded &wr tsizes &CodedIndexKinds.TypeDefOrRef row.EventType
end

type PropertyMap = struct
    interface ISerializer<PropertyMapRow> with
        member _.Serialize(_, tsizes, row, _, _, wr) =
            WriteIndex.table &wr tsizes ValidTableFlags.TypeDef row.Parent
            WriteIndex.table &wr tsizes ValidTableFlags.Property row.PropertyList
end

type Property = struct
    interface ISerializer<PropertyRow> with
        member _.Serialize(hsizes, _, row, _, _, wr) =
            wr.WriteLE(uint16 row.Flags)
            WriteIndex.string &wr hsizes row.Name.Offset
            WriteIndex.blob &wr hsizes row.Type.PropertySig
end

type MethodSemantics = struct
    interface ISerializer<MethodSemanticsRow> with
        member _.Serialize(_, tsizes, row, _, _, wr) =
            wr.WriteLE(uint16 row.Semantics)
            WriteIndex.table &wr tsizes ValidTableFlags.MethodDef row.Method
            WriteIndex.coded &wr tsizes &CodedIndexKinds.HasSemantics row.Association
end

type MethodImpl = struct
    interface ISerializer<MethodImplRow> with
        member _.Serialize(_, tsizes, row, _, _, wr) =
            WriteIndex.table &wr tsizes ValidTableFlags.TypeDef row.Class
            WriteIndex.coded &wr tsizes &CodedIndexKinds.MethodDefOrRef row.MethodBody
            WriteIndex.coded &wr tsizes &CodedIndexKinds.MethodDefOrRef row.MethodDeclaration
end

type ModuleRef = struct
    interface ISerializer<ModuleRefRow> with
        member _.Serialize(hsizes, _, row, _, _, wr) = WriteIndex.string &wr hsizes row.Name.Offset
end

type TypeSpec = struct
    interface ISerializer<TypeSpecRow> with
        member _.Serialize(hsizes, _, row, _, _, wr) = WriteIndex.blob &wr hsizes row.Signature
end



type FieldRva = struct
    interface ISerializer<FieldRvaRow> with
        member _.Serialize(_, tsizes, row, _, embeddedDataRva, wr) =
            let rva: FSharpIL.PortableExecutable.Rva = embeddedDataRva + row.Rva.Value
            wr.WriteLE(uint32 rva)
            WriteIndex.table &wr tsizes ValidTableFlags.Field row.Field
end






type Assembly = struct
    interface ISerializer<AssemblyRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            wr.WriteLE(uint32 row.HashAlgId)
            wr.WriteLE row.MajorVersion
            wr.WriteLE row.MinorVersion
            wr.WriteLE row.BuildNumber
            wr.WriteLE row.RevisionNumber
            wr.WriteLE(uint32 row.Flags)
            WriteIndex.blob &wr hsizes row.PublicKey
            WriteIndex.string &wr hsizes row.Name.Offset.Offset
            WriteIndex.string &wr hsizes row.Culture
end

type AssemblyRef = struct
    interface ISerializer<AssemblyRefRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            wr.WriteLE row.MajorVersion
            wr.WriteLE row.MinorVersion
            wr.WriteLE row.BuildNumber
            wr.WriteLE row.RevisionNumber
            wr.WriteLE(uint32 row.Flags)
            WriteIndex.blob &wr hsizes row.PublicKeyOrToken.Token
            WriteIndex.string &wr hsizes row.Name.Offset.Offset
            WriteIndex.string &wr hsizes row.Culture
            WriteIndex.blob &wr hsizes row.HashValue
end

type File = struct
    interface ISerializer<FileRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            wr.WriteLE(uint32 row.Flags)
            WriteIndex.string &wr hsizes row.Name.Offset.Offset
            WriteIndex.blob &wr hsizes row.HashValue
end






type NestedClass = struct
    interface ISerializer<NestedClassRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            WriteIndex.table &wr tsizes ValidTableFlags.TypeDef row.NestedClass
            WriteIndex.table &wr tsizes ValidTableFlags.TypeDef row.EnclosingClass
end

type GenericParam = struct
    interface ISerializer<GenericParamRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            wr.WriteLE row.Number
            wr.WriteLE(uint16 row.Flags)
            WriteIndex.coded &wr tsizes &CodedIndexKinds.TypeOrMethodDef row.Owner
            WriteIndex.string &wr hsizes row.Name.Offset
end

type MethodSpec = struct
    interface ISerializer<MethodSpecRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            WriteIndex.coded &wr tsizes &CodedIndexKinds.MethodDefOrRef row.Method
            WriteIndex.blob &wr hsizes row.Instantiation.MethodSpec
end

type GenericParamConstraint = struct
    interface ISerializer<GenericParamConstraintRow> with
        member _.Serialize(hsizes, tsizes, row, _, _, wr) =
            WriteIndex.table &wr tsizes ValidTableFlags.GenericParam row.Owner
            WriteIndex.coded &wr tsizes &CodedIndexKinds.TypeDefOrRef row.Constraint
end
