﻿namespace FSharpIL

open System
open System.Collections.Immutable

open FSharpIL.Utilities

type IsDll =
    | Dll
    | Exe

// II.25.2.2.1
/// Flags that specify file characteristics in the PE file header.
[<Flags>]
type ImageFileFlags =
    | FileRelocsStripped = 0x0001us
    | FileExecutableImage = 0x0002us
    | File32BitMachine = 0x0100us // TODO This flag depends on a flag from the CLI header, should it be validated?
    | FileDll = 0x2000us

[<RequireQualifiedAccess>]
type FileCharacteristics =
    | Valid of IsDll
    | Custom of ImageFileFlags

type MachineFlags =
    | I386 = 0x14Cus

// II.25.2.2
type PEFileHeader =
    { Machine: MachineFlags
      // NumberOfSections
      TimeDateStamp: uint32
      SymbolTablePointer: uint32
      SymbolCount: uint32
      // OptionalHeaderSize // TODO: Should this be handled by the writer or should it be customizable. Usually value is 0xE0
      Characteristics: FileCharacteristics }

    /// Default PE file header indicating that the file is a <c>.dll</c> file.
    static member Default =
        { Machine = MachineFlags.I386
          TimeDateStamp = 0u
          SymbolTablePointer = 0u
          SymbolCount = 0u
          Characteristics = FileCharacteristics.Valid Dll }

// II.25.2.3.1
type StandardFields =
    { // Note that the Magic field is not included, so it should always be PE32 (0x10B), not PE32+ (0x20B)
      LMajor: byte
      LMinor: byte
      // CodeSize
      // InitializedDataSize
      // UninitizalizedDataSize
      // EntryPointRva
      // BaseOfCode
      // BaseOfData // Note that this field is absent in PE32+
      }

    static member Default =
        { LMajor = 0uy
          LMinor = 0uy }

type ImageBase =
    | ImageBase of uint16

    static member op_Implicit(ImageBase imageBase) =
        (uint32 imageBase) * 0x10000u

    static member Default = ImageBase 0x40us

type ImageSubsystem =
    | WindowsGui
    | WindowsCui

[<Flags>]
type PEFileFlags =
    | Reserved = 0x000Fus
    | HighEntropyVA = 0x0020us
    | DynamicBase = 0x0040us
    | ForceIntegrity = 0x0080us
    | NXCompatible = 0x0100us
    | NoIsolation = 0x0200us
    | NoSEH = 0x0400us
    | NoBind = 0x0800us
    | AppContainer = 0x1000us
    | WdmDriver = 0x2000us
    | GuardCF = 0x4000us
    | TerminalServerAware = 0x8000us

type AlignmentInfo =
    internal
        { // 0uy indicates an alignment of 512
          Section: byte
          File: byte }

    static member private Convert value =
        pown 2u (int value + 9)

    member this.SectionAlignment = AlignmentInfo.Convert this.Section
    member this.FileAlignment = AlignmentInfo.Convert this.File

    static member Default =
        { Section = 0uy
          File = 0uy }

// II.25.2.3.2
type NTSpecificFields =
    { ImageBase: ImageBase
      Alignment: AlignmentInfo
      OSMajor: uint16
      OSMinor: uint16
      UserMajor: uint16
      UserMinor: uint16
      SubSysMajor: uint16
      SubSysMinor: uint16
      /// Reserved value that should be zero.
      Win32VersionValue: uint32
      // ImageSize
      // HeaderSize
      FileChecksum: uint32
      Subsystem: ImageSubsystem
      DllFlags: PEFileFlags
      // StackReserveSize
      // StackCommitSize
      // HeapReserveSize
      // HeapCommitSize
      /// Reserved value that should be zero.
      LoaderFlags: uint32
      // NumberOfDataDirectories
      }

    static member Default =
        { ImageBase = ImageBase.Default
          Alignment = AlignmentInfo.Default
          OSMajor = 0x05us
          OSMinor = 0us
          UserMajor = 0us
          UserMinor = 0us
          SubSysMajor = 0x05us
          SubSysMinor = 0us
          Win32VersionValue = 0u
          FileChecksum = 0u
          Subsystem = WindowsCui
          DllFlags =
              PEFileFlags.DynamicBase |||
              PEFileFlags.NoSEH |||
              PEFileFlags.NXCompatible
          LoaderFlags = 0u }

// II.25.2.3.3
// TODO: Should data directories and section table be handled by one type instead?
// NOTE: Pointers in the DataDirectories can point to content in different parts of a section!
// NOTE: The RVA is in a weird format, 0x08 0x20 0x00 0x00 means a file offset (not RVA) of 0x208. Search up how to convert RVA to file offset
type DataDirectories =
    { ExportTable: unit
      ImportTable: unit
      ResourceTable: unit
      ExceptionTable: unit
      CertificateTable: unit
      BaseRelocationTable: unit
      DebugTable: unit
      CopyrightTable: unit
      //GlobalPointer // This apparently always has a size of zero.
      TLSTable: unit
      LoadConfigTable: unit
      BoundImportTable: unit
      ImportAddressTable: unit
      DelayImportDescriptor: unit
      // CliHeader
      // Reserved
      }

    static member Default =
        { ExportTable = ()
          ImportTable = ()
          ResourceTable = ()
          ExceptionTable = ()
          CertificateTable = ()
          BaseRelocationTable = ()
          DebugTable = ()
          CopyrightTable = ()
          TLSTable = ()
          LoadConfigTable = ()
          BoundImportTable = ()
          ImportAddressTable = ()
          DelayImportDescriptor = () }

// TODO: figure out what the DataDirectories point to.

// II.25.3.3.1
[<Flags>]
type CliHeaderFlags =
    | StrongNameSigned = 0x8u

type CliMetadataVersion =
    internal
    | CliMetadataVersion of string

    override this.ToString() =
        let (CliMetadataVersion version) = this in version

// II.24.2.2
type CliMetadataStream =
    | MetadataStream
    | StringStream
    | UserStringStream
    | GUIDStream
    | BlobStream

// NOTE: II.24.2.2 says that each type of stream can only occur 1 time at most, so use a Set collection type here.
type CliMetadataStreams = unit

// II.24.2.1
type CliMetadataRoot =
    { // Signature
      MajorVersion: uint16
      MinorVersion: uint16
      // Reserved
      Version: CliMetadataVersion
      // Flags
      Streams: CliMetadataStreams }

    static member Default =
        { MajorVersion = 1us
          MinorVersion = 1us
          Version = CliMetadataVersion "v4.0.30319"
          Streams = () }

// II.25.3.3
type CliHeader =
    { // Cb
      MajorRuntimeVersion: uint16
      MinorRuntimeVersion: uint16
      Metadata: CliMetadataRoot
      Flags: CliHeaderFlags // TODO: Create default value for flags.
      EntryPointToken: unit
      Resources: unit
      StrongNameSignature: unit
      CodeManagerTable: uint64
      VTableFixups: unit
      // ExportAddressTableJumps
      // ManagedNativeHeader
      }

    static member Default =
        { MajorRuntimeVersion = 2us
          MinorRuntimeVersion = 0us
          Metadata = CliMetadataRoot.Default
          Flags = invalidOp "default here"
          EntryPointToken = ()
          Resources = ()
          StrongNameSignature = ()
          CodeManagerTable = 0UL
          VTableFixups = () }

[<Flags>]
type SectionFlags =
    | Code = 0x20u
    | InitializedData = 0x40u
    | UninitializedData = 0x80u
    | Execute = 0x20000000u
    | Read = 0x40000000u
    | Write = 0x80000000u

type SectionData =
    //
    | CliHeader of CliHeader
    | SectionData of
        {| Characteristics: SectionFlags
           RawData: unit |} // TODO: Should raw data be a byte[], ImmutableArray<byte>, other type, or a lazy variation of previous?

type SectionName =
    internal
    | SectionName of string // Maybe use System.Text.Encoding.UTF8.GetBytes, unless UTF8 is not the encoding of the text.

// II.25.3
/// NOTE: Section headers begin after the file headers, but must account for SizeOfHeaders, which is rounded up to a multiple of FileAlignment.
type SectionHeader =
    { SectionName: SectionName
      // VirtualSize: uint32
      // VirtualAddress: uint32
      Data: SectionData
      PointerToRelocations: uint32
      PointerToLineNumbers: uint32
      NumberOfRelocations: uint16
      NumberOfLineNumbers: uint16 }

type SectionTable = ImmutableSortedSet<SectionHeader> // TODO: Choose a collection type that allows accessing by index, and determine if duplicate section names are allowed.

type PEFile =
    { FileHeader: PEFileHeader
      StandardFields: StandardFields
      NTSpecificFields: NTSpecificFields
      DataDirectories: DataDirectories
      SectionTable: SectionTable }

    static member Default =
        { FileHeader = PEFileHeader.Default
          StandardFields = StandardFields.Default
          NTSpecificFields = NTSpecificFields.Default
          DataDirectories = DataDirectories.Default
          SectionTable = invalidOp "what should default be" }
