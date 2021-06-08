namespace FSharpIL.Reading

open FSharpIL.Metadata.Tables

/// Streams refered to by various rows in the metadata tables.
type ReferencedMetadataStreams =
    { Strings: ParsedStringsStream
      Guid: ParsedGuidStream
      //UserString: ParsedUserStringStream // NOTE: This would be needed for reading method bodies.
      Blob: ParsedBlobStream
      Tables: ParsedMetadataTables }

type TableRowReader<'Row, 'State> = StructureReader<struct(ReferencedMetadataStreams * 'Row), 'State>

[<NoComparison; NoEquality>]
type SequentialTableReader<'State> =
    { ReadModule: TableRowReader<ModuleRow, 'State>
      ReadTypeRef: TableRowReader<TypeRefRow, 'State>
      ReadTypeDef: TableRowReader<TypeDefRow, 'State>
      ReadField: TableRowReader<FieldRow, 'State>
      ReadMethodDef: TableRowReader<MethodDefRow, 'State>
      ReadParam: TableRowReader<ParamRow, 'State>
      ReadInterfaceImpl: TableRowReader<InterfaceImplRow, 'State>
      ReadMemberRef: TableRowReader<MemberRefRow, 'State>
      ReadConstant: TableRowReader<ConstantRow, 'State>
      ReadCustomAttribute: TableRowReader<CustomAttributeRow, 'State>
      //ReadFieldMarshal: TableRowReader
      //ReadDeclSecurity: TableRowReader
      ReadClassLayout: TableRowReader<ClassLayoutRow, 'State>
      //FieldLayout: TableRowReader
      ReadStandAloneSig: TableRowReader<StandaloneSigRow, 'State>
      //ReadEventMap: TableRowReader<EventMapRow, 'State>
      //ReadEvent: TableRowReader<EventRow, 'State>
      ReadPropertyMap: TableRowReader<PropertyMapRow, 'State>
      ReadProperty: TableRowReader<PropertyRow, 'State>
      ReadMethodSemantics: TableRowReader<MethodSemanticsRow, 'State>
      ReadMethodImpl: TableRowReader<MethodImplRow, 'State>
      //ReadModuleRef: TableRowReader<ModuleRefRow, 'State>
      ReadTypeSpec: TableRowReader<TypeSpecRow, 'State>
      //ReadImplMap: TableRowReader
      ReadFieldRva: TableRowReader<FieldRvaRow, 'State>
      ReadAssembly: TableRowReader<AssemblyRow, 'State>
      ReadReadRef: TableRowReader<AssemblyRefRow, 'State>
      //ReadFile: TableRowReader<FileRow, 'State>
      //ReadExportedType: TableRowReader<ExportedTypeRow, 'State>
      ReadManifestResource: TableRowReader<ManifestResourceRow, 'State>
      ReadNestedClass: TableRowReader<NestedClassRow, 'State>
      ReadGenericParam: TableRowReader<GenericParamRow, 'State>
      ReadMethodSpec: TableRowReader<MethodSpecRow, 'State>
      ReadGenericParamConstraint: TableRowReader<GenericParamConstraintRow, 'State>
      // TODO: Include debugging rows not covered in latest version of ECMA-335.
      }

type MetadataTablesReader<'State> =
    //| AllAtOnce of (ReferencedMetadataStreams -> FileOffset -> 'State -> Result<'State, ReadError>)
    | SequentialTableReader of SequentialTableReader<'State>
