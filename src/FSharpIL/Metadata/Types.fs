namespace FSharpIL.Metadata

open System
open System.Collections.Immutable

// II.22.30
type ModuleTable =
    { // Generation
      Name: ModuleName
      Mvid: Guid
      // EncId
      // EncBaseId
      }

    static member Default =
        { Name = ModuleName "Default.dll"
          Mvid = Guid.Empty } // TODO: What should the default Mvid be?

// II.22.15
type FieldTable = unit

// II.22.26
type MethodDefTable = unit

// II.22.33
type ParamTable = unit

// II.22.23
type InterfaceImplTable = unit

// II.22.25
type MemberRefTable = unit

// II.22.9
type ConstantTable = unit

// II.22.10
type CustomAttributeTable = unit

// II.22.17
type FieldMarshalTable = unit

// II.22.11
type DeclSecurityTable = unit

// II.22.8
type ClassLayoutTable = unit

// II.22.16
type FieldLayoutTable = unit

// II.22.36
type StandAloneSigTable = unit

// II.22.12
type EventMapTable = unit

// II.22.13
type EventTable = unit

// II.22.35
type PropertyMapTable = unit

// II.22.34
type PropertyTable = unit

// II.22.28
type MethodSemanticsTable = unit

// II.22.27
type MethodImplTable = unit

// II.22.31
type ModuleRefTable = unit

// II.22.39
type TypeSpecTable = unit

// II.22.22
type ImplMapTable = unit

// II.22.18
type FieldRvaTable = unit

// II.22.2
type AssemblyTable =
    { HashAlgId: unit }

    static member Default =
        { HashAlgId = () }

// II.22.5
type AssemblyRefTable = unit

// II.22.19
type FileTable = unit

// II.22.14
type ExportedTypeTable = unit

// II.22.24
type ManifestResourceTable = unit

// II.22.32
type NestedClassTable = unit

// II.22.20
type GenericParamTable = unit

// II.22.29
type MethodSpecTable = unit

// II.22.21
type GenericParamConstraintTable = unit

// II.24.2.6
type MetadataTables =
    { // Reserved: uint32
      MajorVersion: byte
      MinorVersion: byte
      // HeapSizes: byte
      // Reserved: byte
      // Valid: uint64
      // Sorted: uint64 // TODO: Figure out what Sorted is used for.
      // Rows
      Module: ModuleTable // 0x00
      TypeRef: TypeRefTable // TODO: Figure out which tables can be empty (put option unless it is a collection type)
      TypeDef: TypeDefTable
      Field: FieldTable // 0x04
      MethodDef: MethodDefTable // 0x06
      Param: ParamTable // 0x08
      InterfaceImpl: InterfaceImplTable
      MemberRef: MemberRefTable
      Constant: ConstantTable
      CustomAttribute: CustomAttributeTable
      FieldMarshal: FieldMarshalTable
      DeclSecurity: DeclSecurityTable
      ClassLayout: ClassLayoutTable
      FieldLayout: FieldLayoutTable
      StandAloneSig: StandAloneSigTable
      EventMap: EventMapTable
      Event: EventTable // 0x14
      PropertyMap: PropertyMapTable
      Property: PropertyTable // 0x17
      MethodSemantics: MethodSemanticsTable
      MethodImpl: MethodImplTable
      ModuleRef: ModuleRefTable
      TypeSpec: TypeSpecTable
      ImplMap: ImplMapTable
      FieldRva: FieldRvaTable
      Assembly: AssemblyTable option // 0x20
      // AssemblyProcessor
      // AssemblyOS
      AssemblyRef: AssemblyRefTable option // 0x23
      // AssemblyRefProcessor // TODO: Determine if these tables are needed.
      // AssemblyRefOS
      File: FileTable
      ExportedType: ExportedTypeTable
      ManifestResource: ManifestResourceTable
      NestedClass: NestedClassTable
      GenericParam: GenericParamTable
      MethodSpec: MethodSpecTable
      GenericParamConstraint: GenericParamConstraintTable }

    // NOTE: HeapSizes should be set depending on the number of entries in each stream, see II.24.2.6
    // member this.HeapSizes
    // member this.Valid

    // member this.Rows

    static member Default: MetadataTables =
        { MajorVersion = 1uy
          MinorVersion = 1uy
          Module = ModuleTable.Default
          TypeRef = TypeRefTable.empty
          TypeDef = TypeDefTable.empty
          Field = ()
          MethodDef = ()
          Param = ()
          InterfaceImpl = ()
          MemberRef = ()
          Constant = ()
          CustomAttribute = ()
          FieldMarshal = ()
          DeclSecurity = ()
          ClassLayout = ()
          FieldLayout = ()
          StandAloneSig = ()
          EventMap = ()
          Event = ()
          PropertyMap = ()
          Property = ()
          MethodSemantics = ()
          MethodImpl = ()
          ModuleRef = ()
          TypeSpec = ()
          ImplMap = ()
          FieldRva = ()
          Assembly = None // TODO: Figure out if None is a good default value.
          AssemblyRef = None
          File = ()
          ExportedType = ()
          ManifestResource = ()
          NestedClass = ()
          GenericParam = ()
          MethodSpec = ()
          GenericParamConstraint = () }

/// NOTE: II.24.2.2 says that each type of stream can only occur 1 time at most.
type MetadataStreams =
    { /// The `#~` stream.
      Metadata: MetadataTables
      //StringStream: unit
      //UserStringStream: unit
      //GUIDStream: unit
      //BlobStream: unit
      }

    member _.Count = 1us

    static member Default =
        { Metadata = MetadataTables.Default }

// II.24.2.1
type MetadataRoot =
    { // Signature
      MajorVersion: uint16
      MinorVersion: uint16
      // Reserved
      Version: MetadataVersion
      // Flags
      Streams: MetadataStreams }

    static member Default =
        { MajorVersion = 1us
          MinorVersion = 1us
          Version = MetadataVersion "v4.0.30319"
          Streams = MetadataStreams.Default }

// II.25.3.3.1
[<Flags>]
type CorFlags =
    | ILOnly = 0x1u
    | Requires32Bit = 0x2u
    | StrongNameSigned = 0x8u
    | NativeEntryPoint = 0x10u
    | TrackDebugData = 0x10000u

// II.25.3.3
[<ReferenceEquality>]
type CliHeader =
    { // HeaderSize = 0x48u
      MajorRuntimeVersion: uint16
      MinorRuntimeVersion: uint16
      Metadata: MetadataRoot
      Flags: CorFlags // TODO: Create default value for flags.
      Resources: unit
      StrongNameSignature: ImmutableArray<byte>
      CodeManagerTable: uint64 // TODO: Figure out if this field should exist.
      VTableFixups: unit
      // ExportAddressTableJumps
      // ManagedNativeHeader
      }

    member this.EntryPointToken = Unchecked.defaultof<unit>

    static member Default =
        { MajorRuntimeVersion = 2us
          MinorRuntimeVersion = 5us
          Metadata = MetadataRoot.Default
          Flags = CorFlags.ILOnly // TODO: Figure out if this is an appropriate default.
          Resources = ()
          StrongNameSignature = ImmutableArray.Empty
          CodeManagerTable = 0UL
          VTableFixups = () }
