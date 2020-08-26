namespace FSharpIL

open System

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
    | File32BitMachine = 0x0100us
    | FileDll = 0x2000us

[<RequireQualifiedAccess>]
type FileCharacteristics =
    | Valid of IsDll
    | Custom of ImageFileFlags

// II.25.2.2
type PEFileHeader =
    { Machine: uint16
      // NumberOfSections
      TimeDateStamp: uint32
      SymbolTablePointer: uint32
      SymbolCount: uint32
      // OptionalHeaderSize // TODO: Should this be handled by the writer or should it be customizable. Usually value is 0xE0
      Characteristics: FileCharacteristics }

    /// Default PE file header indicating that the file is a <c>.dll</c> file.
    static member Default =
        { Machine = 0x14Cus
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
type ImageDllCharacteristics =
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
      DllFlags: ImageDllCharacteristics
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
              ImageDllCharacteristics.DynamicBase |||
              ImageDllCharacteristics.NoSEH |||
              ImageDllCharacteristics.NXCompatible
          LoaderFlags = 0u }

// II.25.2.3.3
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
      CLIHeader: unit
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
          DelayImportDescriptor = ()
          CLIHeader = () }

type PEFile =
    { FileHeader: PEFileHeader
      StandardFields: StandardFields
      NTSpecificFields: NTSpecificFields
      DataDirectories: DataDirectories }

    static member Default =
        { FileHeader = PEFileHeader.Default
          StandardFields = StandardFields.Default
          NTSpecificFields = NTSpecificFields.Default
          DataDirectories = DataDirectories.Default }
