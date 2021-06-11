namespace FSharpIL.Writing

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.Writing.Tables

type RowBuilder<'Row when 'Row :> ITableRow> = StringsStreamBuilder -> GuidStreamBuilder -> BlobStreamBuilder -> 'Row

/// <summary>Builds the metadata tables stored in the <c>#~</c> metadata stream (II.24.2.6 and II.22).</summary>
[<Sealed>]
type MetadataTablesBuilder (moduleBuilder: RowBuilder<ModuleRow>, strings, guid, blob) as builder =
    let [<Literal>] MaxSmallHeapOffset = 65535u
    let mutable valid, sorted = ValidTableFlags.Module, ValidTableFlags.None

    member _.HeapSizes =
        let mutable flags = HeapSizes.None
        if strings.StreamLength > MaxSmallHeapOffset then flags <- flags ||| HeapSizes.String
        if guid.StreamLength > MaxSmallHeapOffset then flags <- flags ||| HeapSizes.Guid
        if blob.StreamLength > MaxSmallHeapOffset then flags <- flags ||| HeapSizes.Blob
        flags

    /// (0x00)
    member val Module = moduleBuilder strings guid blob
    /// (0x01)
    member val TypeRef = TypeRefTableBuilder()
    /// (0x02)
    member val TypeDef = TypeDefTableBuilder()
    /// (0x04)
    member val Field = ListTableBuilder<FieldRow>() // TryAdd for this table builder type should return a list of all indices added.
    /// (0x06)
    member val MethodDef = ListTableBuilder<MethodDefRow>()
    /// (0x08)
    member val Param = ListTableBuilder<ParamRow>()
    /// (0x09)
    member val InterfaceImpl = InterfaceImplTableBuilder()
    /// (0x0A)
    member val MemberRef = MemberRefTableBuilder()
    /// (0x0B)
    member val Constant = ConstantTableBuilder()
    /// (0x0C)
    member val CustomAttribute = CustomAttributeTableBuilder()
    // (0x0D)
    // member FieldMarshal
    // (0x0E)
    // member DeclSecurity
    // (0x0F)
    // member ClassLayout
    // (0x10)
    // member FieldLayout
    // (0x11)
    member val StandAloneSig = StandaloneSigTableBuilder()
    /// (0x12)
    member val EventMap = OwnedMetadataTableBuilder<TypeDefRow, EventRow>()
    /// (0x14)
    member val Event = noImpl ""
    /// (0x15)
    member val PropertyMap = OwnedMetadataTableBuilder<TypeDefRow, PropertyRow>()
    /// (0x17)
    member val Property = noImpl ""
    /// (0x18)
    member val MethodSemantics = MethodSemanticsTableBuilder()
    /// (0x19)
    member val MethodImpl = MethodImplTableBuilder()
    /// (0x1A)
    member val ModuleRef = ModuleRefTableBuilder()
    /// (0x1B)
    member val TypeSpec = TypeSpecTableBuilder()
    // (0x1C)
    // member ImplMap
    // (0x1D)
    // member FieldRva
    /// <summary>Represents the <c>Assembly</c> table, which describes the current assembly (0x20).</summary>
    member val Assembly = AssemblyTableBuilder()
    // AssemblyProcessor // 0x21 // Not used when writing a PE file
    // AssemblyOS // 0x22 // Not used when writing a PE file
    /// <summary>Represents the <c>AssemblyRef</c> table, which contains references to other assemblies (0x23).</summary>
    member val AssemblyRef = AssemblyRefTableBuilder()
    // AssemblyRefProcessor // 0x24 // Not used when writing a PE file
    // AssemblyRefOS // 0x25 // Not used when writing a PE file
    /// (0x26)
    member val File = FileTableBuilder()
    // (0x27)
    // member ExportedType
    // (0x28)
    // member ManifestResource
    /// (0x29)
    member val NestedClass = NestedClassTableBuilder()
    /// (0x2A)
    member val GenericParam = ListTableBuilder<GenericParamRow>()
    // (0x2B)
    member val MethodSpec = MethodSpecTableBuilder()
    // (0x2C)
    member val GenericParamConstraint = ListTableBuilder<GenericParamConstraintRow>()

    member val IndexSizes =
        { new ITableRowCounts with
            member _.RowCount table =
                match table with
                | ValidTableFlags.Module -> 1u
                | ValidTableFlags.TypeRef -> TableBuilder.count builder.TypeRef
                | ValidTableFlags.TypeDef -> TableBuilder.count builder.TypeDef
                | ValidTableFlags.Field -> TableBuilder.count builder.Field
                | ValidTableFlags.MethodDef -> TableBuilder.count builder.MethodDef
                | ValidTableFlags.Param -> TableBuilder.count builder.Param
                | ValidTableFlags.InterfaceImpl -> TableBuilder.count builder.InterfaceImpl
                | ValidTableFlags.MemberRef -> TableBuilder.count builder.MemberRef
                | ValidTableFlags.Constant -> TableBuilder.count builder.Constant
                | ValidTableFlags.CustomAttribute -> TableBuilder.count builder.CustomAttribute

                | ValidTableFlags.StandAloneSig -> TableBuilder.count builder.StandAloneSig
                | ValidTableFlags.EventMap -> TableBuilder.count builder.EventMap
                | ValidTableFlags.Event -> TableBuilder.count builder.Event
                | ValidTableFlags.PropertyMap -> TableBuilder.count builder.PropertyMap
                | ValidTableFlags.Property -> TableBuilder.count builder.Property
                | ValidTableFlags.MethodSemantics -> TableBuilder.count builder.MethodSemantics
                | ValidTableFlags.MethodImpl -> TableBuilder.count builder.MethodImpl
                | ValidTableFlags.ModuleRef -> TableBuilder.count builder.ModuleRef
                | ValidTableFlags.TypeSpec -> TableBuilder.count builder.TypeSpec

                | ValidTableFlags.Assembly -> TableBuilder.count builder.Assembly
                | ValidTableFlags.AssemblyRef -> TableBuilder.count builder.AssemblyRef
                | ValidTableFlags.File -> TableBuilder.count builder.File

                | ValidTableFlags.NestedClass -> TableBuilder.count builder.NestedClass
                | ValidTableFlags.GenericParam -> TableBuilder.count builder.GenericParam
                | ValidTableFlags.MethodSpec -> TableBuilder.count builder.MethodSpec
                | ValidTableFlags.GenericParamConstraint -> TableBuilder.count builder.GenericParamConstraint
                | _ -> 0u }

    member inline private this.SerializeTable(wr: byref<_>, table: #ITableBuilder<_>) =
        TableBuilder.serialize &wr this.HeapSizes this.IndexSizes table

    interface IStreamBuilder with
        member _.StreamName = Magic.StreamNames.metadata
        member _.StreamLength =
            failwith "TODO: Calculate stream length"
        member this.Serialize wr =
            // TODO: Use TablesHeader<_> type?
            wr.WriteLE 0u // Reserved
            wr.Write 2uy // MajorVersion
            wr.Write 0uy // MinorVersion
            wr.Write(uint8 this.HeapSizes)
            wr.Write 0uy // Reserved
            wr.WriteLE(uint64 valid) // Valid
            wr.WriteLE(uint64 sorted) // Sorted

            // Rows
            for i = 0 to 63 do
                let size = this.IndexSizes.RowCount(LanguagePrimitives.EnumOfValue(1UL <<< i))
                if size > 0u then wr.WriteLE size

            // Module
            wr.WriteLE this.Module.Generation
            StreamOffset.writeString &wr this.HeapSizes this.Module.Name
            StreamOffset.writeGuid &wr this.HeapSizes this.Module.Mvid
            StreamOffset.writeGuid &wr this.HeapSizes this.Module.EncId
            StreamOffset.writeGuid &wr this.HeapSizes this.Module.EncBaseId

            this.SerializeTable(&wr, this.TypeRef)
            this.SerializeTable(&wr, this.TypeDef)
            this.SerializeTable(&wr, this.Field)
            this.SerializeTable(&wr, this.MethodDef)
            this.SerializeTable(&wr, this.Param)
            this.SerializeTable(&wr, this.InterfaceImpl)
            this.SerializeTable(&wr, this.MemberRef)
            this.SerializeTable(&wr, this.Constant)
            this.SerializeTable(&wr, this.CustomAttribute)

            this.SerializeTable(&wr, this.StandAloneSig)
            this.SerializeTable(&wr, this.EventMap)
            this.SerializeTable(&wr, this.Event)
            this.SerializeTable(&wr, this.PropertyMap)
            this.SerializeTable(&wr, this.Property)
            this.SerializeTable(&wr, this.MethodSemantics)
            this.SerializeTable(&wr, this.MethodImpl)
            this.SerializeTable(&wr, this.ModuleRef)
            this.SerializeTable(&wr, this.TypeSpec)

            this.SerializeTable(&wr, this.Assembly)
            this.SerializeTable(&wr, this.AssemblyRef)
            this.SerializeTable(&wr, this.File)

            this.SerializeTable(&wr, this.NestedClass)
            this.SerializeTable(&wr, this.GenericParam)
            this.SerializeTable(&wr, this.MethodSpec)
            this.SerializeTable(&wr, this.GenericParamConstraint)
