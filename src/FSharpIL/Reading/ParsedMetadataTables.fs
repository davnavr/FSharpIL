namespace FSharpIL.Reading

open System.Collections.Generic

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata.Tables

type ParsedTablesHeader = TablesHeader<ParsedTableRowCounts>

[<Sealed>]
type MetadataTableParser<'RowParser, 'Row
    when 'RowParser :> IByteParser<'Row>
    and 'RowParser : struct
    and 'Row : struct
    and 'Row :> ITableRow>
    internal
    (
        offset: uint32,
        rows: ChunkedMemory,
        parser: 'RowParser
    )
    =
    static member val Empty = MetadataTableParser<'RowParser, 'Row>(0u, ChunkedMemory.empty, Unchecked.defaultof<'RowParser>)

    member _.IsEmpty = rows.IsEmpty
    member _.RowCount = rows.Length / parser.Length
    /// The length of one row in this metadata table, in bytes.
    member _.RowLength = parser.Length
    /// The length of this metadata table, in bytes.
    member _.Length = rows.Length
    /// Offset from the first byte of the table rows to the first byte of the first row of this table.
    member _.TableOffset = offset

    /// <summary>Attempts to retrieve the row at the specified index.</summary>
    /// <returns>
    /// The row at the specified <paramref name="index"/>, or <c>ValueNone</c> if the index was out of bounds or if the index was
    /// zero.
    /// </returns>
    member this.TryGetRow(index: TableIndex<'Row>) =
        match index with
        | { TableIndex = i } when i <= this.RowCount && i > 0u ->
            ValueSome(ByteParser.parse ((i - 1u) * parser.Length) &rows parser)
        | _ -> ValueNone

    /// <summary>Retrieves the row at the specified index.</summary>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the specified index was out of bounds or zero.
    /// </exception>
    member this.Item with get index =
        match this.TryGetRow index with
        | ValueSome i -> i
        | ValueNone ->
            argOutOfRange
                "index"
                index
                (sprintf "The table index must not be null and cannot be greater than 0x%08X" this.RowCount)

type ModuleTableParser = MetadataTableParser<ModuleParser, ModuleRow>
type TypeRefTableParser = MetadataTableParser<TypeRefParser, TypeRefRow>
type TypeDefTableParser = MetadataTableParser<TypeDefParser, TypeDefRow>
type FieldTableParser = MetadataTableParser<FieldParser, FieldRow>
type MethodDefTableParser = MetadataTableParser<MethodDefParser, MethodDefRow>
type ParamTableParser = MetadataTableParser<ParamParser, ParamRow>
type InterfaceImplTableParser = MetadataTableParser<InterfaceImplParser, InterfaceImplRow>
type MemberRefTableParser = MetadataTableParser<MemberRefParser, MemberRefRow>
type ConstantTableParser = MetadataTableParser<ConstantParser, ConstantRow>
type CustomAttributeTableParser = MetadataTableParser<CustomAttributeParser, CustomAttributeRow>

type ClassLayoutTableParser = MetadataTableParser<ClassLayoutParser, ClassLayoutRow>

type StandaloneSigTableParser = MetadataTableParser<StandaloneSigParser, StandaloneSigRow>

type PropertyMapTableParser = MetadataTableParser<PropertyMapParser, PropertyMapRow>
type PropertyTableParser = MetadataTableParser<PropertyParser, PropertyRow>
type MethodSemanticsTableParser = MetadataTableParser<MethodSemanticsParser, MethodSemanticsRow>
type MethodImplTableParser = MetadataTableParser<MethodImplParser, MethodImplRow>
type TypeSpecTableParser = MetadataTableParser<TypeSpecParser, TypeSpecRow>

type FieldRvaTableParser = MetadataTableParser<FieldRvaParser, FieldRvaRow>
type AssemblyTableParser = MetadataTableParser<AssemblyParser, AssemblyRow>
type AssemblyRefTableParser = MetadataTableParser<AssemblyRefParser, AssemblyRefRow>

type ManifestResourceTableParser = MetadataTableParser<ManifestResourceParser, ManifestResourceRow>
type NestedClassTableParser = MetadataTableParser<NestedClassParser, NestedClassRow>
type GenericParamTableParser = MetadataTableParser<GenericParamParser, GenericParamRow>
type MethodSpecTableParser = MetadataTableParser<MethodSpecParser, MethodSpecRow>
type GenericParamConstraintTableParser = MetadataTableParser<GenericParamConstraintParser, GenericParamConstraintRow>
//type TemporaryTableParser = MetadataTableParser<MyRow, MyParser>

[<NoComparison; NoEquality>]
type ParsedMetadataTables =
    private
        { rows: ChunkedMemory
          header: ParsedTablesHeader
          mutable moduleTable: ModuleTableParser
          mutable typeRefTable: TypeRefTableParser
          mutable typeDefTable: TypeDefTableParser
          mutable fieldTable: FieldTableParser
          mutable methodDefTable: MethodDefTableParser
          mutable paramTable: ParamTableParser
          mutable interfaceImplTable: InterfaceImplTableParser
          mutable memberRefTable: MemberRefTableParser
          mutable constantTable: ConstantTableParser
          mutable customAttributeTable: CustomAttributeTableParser

          mutable classLayoutTable: ClassLayoutTableParser

          mutable standAloneSigTable: StandaloneSigTableParser

          mutable propertyMapTable: PropertyMapTableParser
          mutable propertyTable: PropertyTableParser
          mutable methodSemanticsTable: MethodSemanticsTableParser
          mutable methodImplTable: MethodImplTableParser
          mutable typeSpecTable: TypeSpecTableParser

          mutable fieldRvaTable: FieldRvaTableParser
          mutable assemblyTable: AssemblyTableParser
          mutable assemblyRefTable: AssemblyRefTableParser

          mutable manifestResourceTable: ManifestResourceTableParser
          mutable nestedClassTable: NestedClassTableParser
          mutable genericParamTable: GenericParamTableParser
          mutable methodSpecTable: MethodSpecTableParser
          mutable genericParamConstraintTable: GenericParamConstraintTableParser
          //mutable myTable: MyTableParser
          }

    member this.Header = this.header
    member this.Module = this.moduleTable

[<RequireQualifiedAccess>]
module ParsedMetadataTables =
    let private createMetadataTable
        offset
        (stream: inref<ChunkedMemory>)
        tablei
        count
        (table: outref<_>)
        (parser: #IByteParser<_>)
        =
        let length = parser.Length * count
        match stream.TrySlice(offset, length) with
        | true, rows ->
            table <- MetadataTableParser(offset, rows, parser)
            Ok length
        | false, _ -> Error(offset, StructureOutOfBounds(ParsedMetadataStructure.MetadataTable tablei))

    let rec private createTablesLoop offset (enumerator: byref<IEnumerator<_>>) tables =
        if enumerator.MoveNext() then
            let (KeyValue(table, count)) = enumerator.Current

            let inline checkTableHasOneRow() =
                match count with
                | 0u -> Some(offset, TableIsEmpty table)
                | 1u -> None
                | _ -> Some(offset, TableHasMoreThanOneRow(table, count))

            let result =
                match table with
                | ValidTableFlags.Module ->
                    match checkTableHasOneRow() with
                    | None ->
                        createMetadataTable
                            offset
                            &tables.rows
                            table
                            count
                            &tables.moduleTable
                            (ModuleParser tables.header.HeapSizes)
                    | Some err -> Error err
                | ValidTableFlags.TypeRef ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.typeRefTable
                        (TypeRefParser(tables.header.HeapSizes, tables.header.Rows))
                | ValidTableFlags.TypeDef ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.typeDefTable
                        (TypeDefParser(tables.header.HeapSizes, tables.header.Rows))
                | ValidTableFlags.Field ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.fieldTable
                        (FieldParser tables.header.HeapSizes)
                | ValidTableFlags.MethodDef ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.methodDefTable
                        (MethodDefParser(tables.header.HeapSizes, tables.header.Rows))
                | ValidTableFlags.Param ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.paramTable
                        (ParamParser tables.header.HeapSizes)
                | ValidTableFlags.InterfaceImpl ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.interfaceImplTable
                        (InterfaceImplParser tables.header.Rows)
                | ValidTableFlags.MemberRef ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.memberRefTable
                        (MemberRefParser(tables.header.HeapSizes, tables.header.Rows))
                | ValidTableFlags.Constant ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.constantTable
                        (ConstantParser(tables.header.HeapSizes, tables.header.Rows))
                | ValidTableFlags.CustomAttribute ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.customAttributeTable
                        (CustomAttributeParser(tables.header.HeapSizes, tables.header.Rows))



                | ValidTableFlags.ClassLayout ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.classLayoutTable
                        (ClassLayoutParser tables.header.Rows)



                | ValidTableFlags.StandAloneSig ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.standAloneSigTable
                        (StandaloneSigParser tables.header.HeapSizes)



                | ValidTableFlags.PropertyMap ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.propertyMapTable
                        (PropertyMapParser tables.header.Rows)
                | ValidTableFlags.Property ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.propertyTable
                        (PropertyParser tables.header.HeapSizes)
                | ValidTableFlags.MethodSemantics ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.methodSemanticsTable
                        (MethodSemanticsParser tables.header.Rows)
                | ValidTableFlags.MethodImpl ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.methodImplTable
                        (MethodImplParser tables.header.Rows)



                | ValidTableFlags.TypeSpec ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.typeSpecTable
                        (TypeSpecParser tables.header.HeapSizes)
                | ValidTableFlags.FieldRva ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.fieldRvaTable
                        (FieldRvaParser tables.header.Rows)
                | ValidTableFlags.Assembly ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.assemblyTable
                        (AssemblyParser tables.header.HeapSizes)
                | ValidTableFlags.AssemblyRef ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.assemblyRefTable
                        (AssemblyRefParser tables.header.HeapSizes)



                | ValidTableFlags.ManifestResource ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.manifestResourceTable
                        (ManifestResourceParser(tables.header.HeapSizes, tables.header.Rows))
                | ValidTableFlags.NestedClass ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.nestedClassTable
                        (NestedClassParser tables.header.Rows)
                | ValidTableFlags.GenericParam ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.genericParamTable
                        (GenericParamParser(tables.header.HeapSizes, tables.header.Rows))
                | ValidTableFlags.MethodSpec ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.methodSpecTable
                        (MethodSpecParser(tables.header.HeapSizes, tables.header.Rows))
                | ValidTableFlags.GenericParamConstraint ->
                    createMetadataTable
                        offset
                        &tables.rows
                        table
                        count
                        &tables.genericParamConstraintTable
                        (GenericParamConstraintParser tables.header.Rows)
                | _ -> failwithf "Table %A is currently not supported" table

            match result with
            | Ok length -> createTablesLoop (offset + length) &enumerator tables
            | Error err -> Error err
        else Ok tables

    let tryCreate (rows: inref<_>) (header: ParsedTablesHeader) =
        let mutable enumerator = header.Rows.GetEnumerator()
        createTablesLoop
            0u
            &enumerator
            { rows = rows
              header = header
              moduleTable = MetadataTableParser.Empty
              typeRefTable = MetadataTableParser.Empty
              typeDefTable = MetadataTableParser.Empty
              fieldTable = MetadataTableParser.Empty
              methodDefTable = MetadataTableParser.Empty
              paramTable = MetadataTableParser.Empty
              interfaceImplTable = MetadataTableParser.Empty
              memberRefTable = MetadataTableParser.Empty
              constantTable = MetadataTableParser.Empty
              customAttributeTable = MetadataTableParser.Empty

              classLayoutTable = MetadataTableParser.Empty

              standAloneSigTable = MetadataTableParser.Empty

              propertyMapTable = MetadataTableParser.Empty
              propertyTable = MetadataTableParser.Empty
              methodSemanticsTable = MetadataTableParser.Empty
              methodImplTable = MetadataTableParser.Empty
              typeSpecTable = MetadataTableParser.Empty

              fieldRvaTable = MetadataTableParser.Empty
              assemblyTable = MetadataTableParser.Empty
              assemblyRefTable = MetadataTableParser.Empty

              manifestResourceTable = MetadataTableParser.Empty
              nestedClassTable = MetadataTableParser.Empty
              genericParamTable = MetadataTableParser.Empty
              methodSpecTable = MetadataTableParser.Empty
              genericParamConstraintTable = MetadataTableParser.Empty
              // myTable = MetadataTableParser.Empty
              }
