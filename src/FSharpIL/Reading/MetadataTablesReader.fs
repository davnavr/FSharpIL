namespace FSharpIL.Reading

open FSharpIL.Metadata.Tables

/// Streams refered to by various rows in the metadata tables.
type ReferencedMetadataStreams =
    { Strings: ParsedStringsStream
      Guid: ParsedGuidStream
      UserString: ParsedUserStringStream
      Blob: ParsedBlobStream
      Tables: ParsedMetadataTables }

// TODO: Allow function to return a ReadError/BlobError
// TODO: Remove sequential reader
// TODO: Make a module called BrowseCli that returns the ReferencedMetadataStreams.
//[<System.ObsoleteAttribute>]
type TableRowReader<'Row, 'State> = StructureReader<struct(ReferencedMetadataStreams * 'Row), 'State> // ('Row -> ReferencedMetadataStreams -> FSharpIL.PortableExecutable.FileOffset -> 'State -> 'State voption) voption

//[<System.ObsoleteAttribute>]
[<NoComparison; NoEquality>]
type SequentialTableReader<'State> =
    { ReadHeader: StructureReader<ParsedTablesHeader, 'State>
      ReadModule: TableRowReader<ModuleRow, 'State>
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
      ReadStandaloneSig: TableRowReader<StandaloneSigRow, 'State>
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
      ReadAssemblyRef: TableRowReader<AssemblyRefRow, 'State>
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
    /// The tables are read in the order that they appear.
    | [<System.ObsoleteAttribute>] SequentialTableReader of SequentialTableReader<'State>

[<RequireQualifiedAccess>]
module MetadataTablesReader =
    [<System.ObsoleteAttribute>]
    let defaultSequentialReader<'State> =
        { ReadHeader = ValueNone
          ReadModule = ValueNone
          ReadTypeRef = ValueNone
          ReadTypeDef = ValueNone
          ReadField = ValueNone
          ReadMethodDef = ValueNone
          ReadParam = ValueNone
          ReadInterfaceImpl = ValueNone
          ReadMemberRef = ValueNone
          ReadConstant = ValueNone
          ReadCustomAttribute = ValueNone
          //ReadFieldMarshal = ValueNone
          //ReadDeclSecurity = ValueNone
          ReadClassLayout = ValueNone
          //FieldLayout = ValueNone
          ReadStandaloneSig = ValueNone
          //ReadEventMap = ValueNone
          //ReadEvent = ValueNone
          ReadPropertyMap = ValueNone
          ReadProperty = ValueNone
          ReadMethodSemantics = ValueNone
          ReadMethodImpl = ValueNone
          //ReadModuleRef = ValueNone
          ReadTypeSpec = ValueNone
          //ReadImplMap = ValueNone
          ReadFieldRva = ValueNone
          ReadAssembly = ValueNone
          ReadAssemblyRef = ValueNone
          //ReadFile = ValueNone
          //ReadExportedType = ValueNone
          ReadManifestResource = ValueNone
          ReadNestedClass = ValueNone
          ReadGenericParam = ValueNone
          ReadMethodSpec = ValueNone
          ReadGenericParamConstraint = ValueNone
        }
        : SequentialTableReader<'State>
