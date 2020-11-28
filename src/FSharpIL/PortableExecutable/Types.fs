namespace FSharpIL.PortableExecutable

open System

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

type FileCharacteristics =
    | FileType of IsDll
    | FileFlags of ImageFileFlags

    static member op_Implicit(characteristics: FileCharacteristics) =
        match characteristics with
        | FileType Exe -> ImageFileFlags.FileExecutableImage
        | FileType Dll -> ImageFileFlags.FileExecutableImage ||| ImageFileFlags.FileDll
        | FileFlags flags -> flags
        |> uint16

type MachineFlags =
    | I386 = 0x14Cus

// II.25.2.2
type CoffHeader =
    { Machine: MachineFlags
      // NumberOfSections
      TimeDateStamp: uint32
      SymbolTablePointer: uint32
      SymbolCount: uint32
      // OptionalHeaderSize
      Characteristics: FileCharacteristics }

    /// Default PE file header indicating that the file is a <c>.dll</c> file.
    static member Default =
        { Machine = MachineFlags.I386
          TimeDateStamp = 0u
          SymbolTablePointer = 0u
          SymbolCount = 0u
          Characteristics = FileType Dll }

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
        { LMajor = 8uy
          LMinor = 0uy }

type ImageBase =
    | ImageBase of uint16

    static member op_Implicit(ImageBase imageBase) =
        (uint32 imageBase) * 0x10000u

    static member Default = ImageBase 0x40us

type ImageSubsystem =
    | Unknown = 0uy
    | Native = 1uy
    | WindowsGui = 2uy
    | WindowsCui = 3uy
    | OS2Cui = 5uy
    | PosixCui = 7uy
    | NativeWindows = 8uy
    | WindowsCEGui = 9uy
    | EfiApplication = 10uy
    | EfiBootServiceDriver = 11uy
    | EfiRuntimeDriver = 12uy
    | EfiRom = 13uy
    | Xbox = 14uy
    | WindowsBootApplication = 16uy

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

// II.25.2.3.2
type NTSpecificFields =
    { ImageBase: ImageBase
      Alignment: Alignment
      OSMajor: uint16
      OSMinor: uint16
      UserMajor: uint16
      UserMinor: uint16
      SubSysMajor: uint16
      SubSysMinor: uint16
      /// Reserved value that must be zero.
      Win32VersionValue: uint32
      // ImageSize
      // HeaderSize
      FileChecksum: uint32
      Subsystem: ImageSubsystem
      DllFlags: PEFileFlags
      StackReserveSize: uint32
      StackCommitSize: uint32
      HeapReserveSize: uint32
      HeapCommitSize: uint32
      /// Reserved value that must be zero.
      LoaderFlags: uint32
      // NumberOfDataDirectories
      }

    static member Default =
        { ImageBase = ImageBase.Default
          Alignment = Alignment.Default
          // NOTE: OSMajor and SubSysMajor both have values of 0x04 in some assemblies
          OSMajor = 0x05us
          OSMinor = 0us
          UserMajor = 0us
          UserMinor = 0us
          SubSysMajor = 0x05us
          SubSysMinor = 0us
          Win32VersionValue = 0u
          FileChecksum = 0u
          Subsystem = ImageSubsystem.WindowsCui
          DllFlags =
              PEFileFlags.DynamicBase |||
              PEFileFlags.NoSEH |||
              PEFileFlags.NXCompatible
          StackReserveSize = 0x100000u
          StackCommitSize = 0x1000u
          HeapReserveSize = 0x100000u
          HeapCommitSize = 0x1000u
          LoaderFlags = 0u }

type PEHeaders =
    { FileHeader: CoffHeader
      StandardFields: StandardFields
      NTSpecificFields: NTSpecificFields }

    static member Default =
      { FileHeader = CoffHeader.Default
        StandardFields = StandardFields.Default
        NTSpecificFields = NTSpecificFields.Default }

type PEFile =
    { Headers: PEHeaders
      SectionInfo: SectionInfo }

    member this.DataDirectories = this.SectionInfo.DataDirectories
    member this.SectionTable = this.SectionInfo.DataDirectories
    //member this.CliHeader = this.

    static member Default =
        { Headers = PEHeaders.Default
          SectionInfo = SectionInfo.Default }
