namespace FSharpIL.Writing

open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

type RowBuilder<'Row when 'Row :> ITableRow> = StringsStreamBuilder -> GuidStreamBuilder -> BlobStreamBuilder -> 'Row

[<Sealed>]
type RowTableBuilder<'Row, 'Serializer
    when 'Row :> ITableRow
    and 'Serializer :> RowSerializers.ISerializer<'Row>
    and 'Row : struct>
    internal
    (
        valid: ValidTableFlags ref,
        table: ValidTableFlags
    )
    =
    let rows = ImmutableArray.CreateBuilder<'Row>()

    member _.Table = table
    member this.IsEmpty = this.Count = 0
    member _.Count = rows.Count

    /// Gets or sets the row at the specified index into the metadata table.
    member _.Item with get ({ TableIndex = i }: TableIndex<'Row>) = &rows.ItemRef(int32(i - 1u))

    /// Adds the row at the specified address to the metadata table.
    member this.Add(row: inref<'Row>): TableIndex<'Row> =
        if this.IsEmpty then valid := !valid ||| table
        rows.Add row
        { TableIndex = uint32 this.Count }

     /// Adds the specified row to the metadata table.
    member this.Add(row: 'Row) = this.Add &row

/// <summary>Builds the metadata tables stored in the <c>#~</c> metadata stream (II.24.2.6 and II.22).</summary>
[<Sealed>]
type MetadataTablesBuilder (moduleRowBuilder: RowBuilder<ModuleRow>, strings, guid, blob) as builder =
    static let [<Literal>] MaxSmallHeapOffset = 65535u
    let valid = ref ValidTableFlags.Module

    member _.Valid = !valid

    member val Sorted = ValidTableFlags.None with get, set

    member _.HeapSizes =
        let mutable flags = HeapSizes.None
        if strings.StreamLength > MaxSmallHeapOffset then flags <- flags ||| HeapSizes.String
        if guid.StreamLength > MaxSmallHeapOffset then flags <- flags ||| HeapSizes.Guid
        if blob.StreamLength > MaxSmallHeapOffset then flags <- flags ||| HeapSizes.Blob
        flags

    /// (0x00)
    member val Module = moduleRowBuilder strings guid blob
    /// (0x01)
    member val TypeRef = RowTableBuilder<_, RowSerializers.TypeRef>(valid, ValidTableFlags.TypeRef)
    /// (0x02)
    member val TypeDef = RowTableBuilder<_, RowSerializers.TypeDef>(valid, ValidTableFlags.TypeDef)
    /// (0x04)
    member val Field = RowTableBuilder<_, RowSerializers.Field>(valid, ValidTableFlags.Field)
    /// (0x06)
    member val MethodDef = RowTableBuilder<_, RowSerializers.MethodDef>(valid, ValidTableFlags.MethodDef)
    /// (0x08)
    member val Param = RowTableBuilder<_, RowSerializers.Param>(valid, ValidTableFlags.Param)
    /// (0x09)
    member val InterfaceImpl = RowTableBuilder<_, RowSerializers.InterfaceImpl>(valid, ValidTableFlags.InterfaceImpl)
    /// (0x0A)
    member val MemberRef = RowTableBuilder<_, RowSerializers.MemberRef>(valid, ValidTableFlags.MemberRef)
    /// (0x0B)
    member val Constant = RowTableBuilder<_, RowSerializers.Constant>(valid, ValidTableFlags.Constant)
    /// (0x0C)
    member val CustomAttribute = RowTableBuilder<_, RowSerializers.CustomAttribute>(valid, ValidTableFlags.CustomAttribute)
    // (0x0D)
    // member FieldMarshal
    // (0x0E)
    // member DeclSecurity
    // (0x0F)
    member val ClassLayout = RowTableBuilder<_, RowSerializers.ClassLayout>(valid, ValidTableFlags.ClassLayout)
    // (0x10)
    // member FieldLayout
    // (0x11)
    member val StandAloneSig = RowTableBuilder<_, RowSerializers.StandAloneSig>(valid, ValidTableFlags.StandAloneSig)
    /// (0x12)
    member val EventMap = RowTableBuilder<_, RowSerializers.EventMap>(valid, ValidTableFlags.EventMap)
    /// (0x14)
    member val Event = RowTableBuilder<_, RowSerializers.Event>(valid, ValidTableFlags.Event)
    /// (0x15)
    member val PropertyMap = RowTableBuilder<_, RowSerializers.PropertyMap>(valid, ValidTableFlags.PropertyMap)
    /// (0x17)
    member val Property = RowTableBuilder<_, RowSerializers.Property>(valid, ValidTableFlags.Property)
    /// (0x18)
    member val MethodSemantics = RowTableBuilder<_, RowSerializers.MethodSemantics>(valid, ValidTableFlags.MethodSemantics)
    /// (0x19)
    member val MethodImpl = RowTableBuilder<_, RowSerializers.MethodImpl>(valid, ValidTableFlags.MethodImpl)
    /// (0x1A)
    member val ModuleRef = RowTableBuilder<_, RowSerializers.ModuleRef>(valid, ValidTableFlags.ModuleRef)
    /// (0x1B)
    member val TypeSpec = RowTableBuilder<_, RowSerializers.TypeSpec>(valid, ValidTableFlags.TypeSpec)
    // (0x1C)
    // member ImplMap
    /// (0x1D)
    member val FieldRva = RowTableBuilder<_, RowSerializers.FieldRva>(valid, ValidTableFlags.FieldRva)
    /// <summary>Represents the <c>Assembly</c> table, which describes the current assembly (0x20).</summary>
    member val Assembly = RowTableBuilder<_, RowSerializers.Assembly>(valid, ValidTableFlags.Assembly)
    // AssemblyProcessor // 0x21 // Not used when writing a PE file
    // AssemblyOS // 0x22 // Not used when writing a PE file
    /// <summary>Represents the <c>AssemblyRef</c> table, which contains references to other assemblies (0x23).</summary>
    member val AssemblyRef = RowTableBuilder<_, RowSerializers.AssemblyRef>(valid, ValidTableFlags.AssemblyRef)
    // AssemblyRefProcessor // 0x24 // Not used when writing a PE file
    // AssemblyRefOS // 0x25 // Not used when writing a PE file
    /// (0x26)
    member val File = RowTableBuilder<_, RowSerializers.File>(valid, ValidTableFlags.File)
    // (0x27)
    // member ExportedType
    // (0x28)
    // member ManifestResource
    /// (0x29)
    member val NestedClass = RowTableBuilder<_, RowSerializers.NestedClass>(valid, ValidTableFlags.NestedClass)
    /// (0x2A)
    member val GenericParam = RowTableBuilder<_, RowSerializers.GenericParam>(valid, ValidTableFlags.GenericParam)
    // (0x2B)
    member val MethodSpec = RowTableBuilder<_, RowSerializers.MethodSpec>(valid, ValidTableFlags.MethodSpec)
    // (0x2C)
    member val GenericParamConstraint = RowTableBuilder<_, RowSerializers.GenericParamConstraint>(valid, ValidTableFlags.GenericParamConstraint)

    member val IndexSizes =
        { new ITableRowCounts with
            member _.RowCount table =
                let inline count (table: RowTableBuilder<_, _>) = uint32 table.Count
                match table with
                | ValidTableFlags.Module -> 1u
                | ValidTableFlags.TypeRef -> count builder.TypeRef
                | ValidTableFlags.TypeDef -> count builder.TypeDef
                | ValidTableFlags.Field -> count builder.Field
                | ValidTableFlags.MethodDef -> count builder.MethodDef
                | ValidTableFlags.Param -> count builder.Param
                | ValidTableFlags.InterfaceImpl -> count builder.InterfaceImpl
                | ValidTableFlags.MemberRef -> count builder.MemberRef
                | ValidTableFlags.Constant -> count builder.Constant
                | ValidTableFlags.CustomAttribute -> count builder.CustomAttribute

                | ValidTableFlags.ClassLayout -> count builder.ClassLayout

                | ValidTableFlags.StandAloneSig -> count builder.StandAloneSig
                | ValidTableFlags.EventMap -> count builder.EventMap
                | ValidTableFlags.Event -> count builder.Event
                | ValidTableFlags.PropertyMap -> count builder.PropertyMap
                | ValidTableFlags.Property -> count builder.Property
                | ValidTableFlags.MethodSemantics -> count builder.MethodSemantics
                | ValidTableFlags.MethodImpl -> count builder.MethodImpl
                | ValidTableFlags.ModuleRef -> count builder.ModuleRef
                | ValidTableFlags.TypeSpec -> count builder.TypeSpec

                | ValidTableFlags.FieldRva -> count builder.FieldRva
                | ValidTableFlags.Assembly -> count builder.Assembly
                | ValidTableFlags.AssemblyRef -> count builder.AssemblyRef
                | ValidTableFlags.File -> count builder.File

                | ValidTableFlags.NestedClass -> count builder.NestedClass
                | ValidTableFlags.GenericParam -> count builder.GenericParam
                | ValidTableFlags.MethodSpec -> count builder.MethodSpec
                | ValidTableFlags.GenericParamConstraint -> count builder.GenericParamConstraint
                | _ -> 0u }

    member private this.SerializeTable(wr: byref<_>, table: RowTableBuilder<_, 'Serializer>, methodBodiesRva, embeddedDataRva) =
        for i = 1 to table.Count do
            Unchecked.defaultof<'Serializer>.Serialize (
                this.HeapSizes,
                this.IndexSizes,
                &table.[{ TableIndex = uint32 i }],
                methodBodiesRva,
                embeddedDataRva,
                &wr
            )

    interface IStreamBuilder with
        member _.StreamName = Magic.StreamNames.metadata
        member _.StreamLength = ValueNone
        member this.Serialize(wr, methodBodiesRva, embeddedDataRva) =
            // TODO: Use TablesHeader<_> type?
            wr.WriteLE 0u // Reserved
            wr.Write 2uy // MajorVersion
            wr.Write 0uy // MinorVersion
            wr.Write(uint8 this.HeapSizes)
            wr.Write 0uy // Reserved
            wr.WriteLE(uint64 !valid)
            wr.WriteLE(uint64 this.Sorted)

            // Rows
            for i = 0 to 63 do
                let size = this.IndexSizes.RowCount(LanguagePrimitives.EnumOfValue(1UL <<< i))
                if size > 0u then wr.WriteLE size

            // Module
            wr.WriteLE this.Module.Generation
            WriteIndex.string &wr this.HeapSizes this.Module.Name.Offset
            WriteIndex.guid &wr this.HeapSizes this.Module.Mvid
            WriteIndex.guid &wr this.HeapSizes this.Module.EncId
            WriteIndex.guid &wr this.HeapSizes this.Module.EncBaseId

            this.SerializeTable(&wr, this.TypeRef, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.TypeDef, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.Field, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.MethodDef, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.Param, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.InterfaceImpl, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.MemberRef, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.Constant, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.CustomAttribute, methodBodiesRva, embeddedDataRva)

            this.SerializeTable(&wr, this.ClassLayout, methodBodiesRva, embeddedDataRva)

            this.SerializeTable(&wr, this.StandAloneSig, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.EventMap, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.Event, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.PropertyMap, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.Property, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.MethodSemantics, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.MethodImpl, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.ModuleRef, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.TypeSpec, methodBodiesRva, embeddedDataRva)

            this.SerializeTable(&wr, this.FieldRva, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.Assembly, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.AssemblyRef, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.File, methodBodiesRva, embeddedDataRva)

            this.SerializeTable(&wr, this.NestedClass, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.GenericParam, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.MethodSpec, methodBodiesRva, embeddedDataRva)
            this.SerializeTable(&wr, this.GenericParamConstraint, methodBodiesRva, embeddedDataRva)
