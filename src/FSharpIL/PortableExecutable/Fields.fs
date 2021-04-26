namespace FSharpIL.PortableExecutable

open System

[<Obsolete>]
type IsDll =
    | [<Obsolete>] IsDll
    | [<Obsolete>] IsExe

/// Flags that specify file characteristics in the PE file header (II.25.2.2.1).
[<Flags>]
type ImageFileFlags =
    | FileRelocsStripped = 0x0001us
    | FileExecutableImage = 0x0002us
    | File32BitMachine = 0x0100us // TODO This flag depends on a flag from the CLI header, should it be validated?
    | FileDll = 0x2000us

[<RequireQualifiedAccess>]
module ImageFileFlags =
    let dll = ImageFileFlags.FileExecutableImage ||| ImageFileFlags.FileDll
    let exe = ImageFileFlags.FileExecutableImage

type MachineFlags =
    | I386 = 0x14Cus

/// (II.25.2.2)
type CoffHeader<'NumSections, 'HeaderSize> =
    { Machine: MachineFlags
      NumberOfSections: 'NumSections
      TimeDateStamp: uint32
      SymbolTablePointer: uint32
      SymbolCount: uint32
      OptionalHeaderSize: 'HeaderSize
      Characteristics: ImageFileFlags }

[<RequireQualifiedAccess>]
module CoffHeader =
    /// <summary>Default PE file header indicating that the file is a <c>.dll</c> file.</summary>
    let defaultFields =
        { Machine = MachineFlags.I386
          NumberOfSections = Omitted
          TimeDateStamp = 0u
          SymbolTablePointer = 0u
          SymbolCount = 0u
          OptionalHeaderSize = Omitted
          Characteristics = ImageFileFlags.dll }

/// <summary>Represents the value of the <c>Magic</c> field in the PE file standard fields.</summary>
type PEImageKind =
    | ROM = 0x107us
    /// Identifies the Portable Executable file as a PE32 executable.
    | PE32 = 0x10Bus
    | PE32Plus = 0x20Bus

/// (II.25.2.3.1)
type StandardFields<'Magic, 'Size, 'BaseOfData> =
    { Magic: 'Magic // Currently, the value always used is PE32 (0x10B), not PE32+ (0x20B)
      LMajor: byte
      LMinor: byte
      CodeSize: 'Size
      InitializedDataSize: 'Size
      UninitializedDataSize: 'Size
      EntryPointRva: 'Size
      BaseOfCode: 'Size
      // Note that this field is absent in PE32+
      BaseOfData: 'BaseOfData }

[<RequireQualifiedAccess>]
module StandardFields =
    let defaultFields =
        { Magic = Omitted
          LMajor = 8uy
          LMinor = 0uy
          CodeSize = Omitted
          InitializedDataSize = Omitted
          UninitializedDataSize = Omitted
          EntryPointRva = Omitted
          BaseOfCode = Omitted
          BaseOfData = Omitted }

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ImageBase = struct
    val internal Value: uint16
    new (value) = { Value = value }
    static member op_Implicit(imageBase: ImageBase) = uint32 imageBase.Value * 0x10000u
    static member op_Implicit(imageBase: ImageBase) = uint64(uint32 imageBase)
    static member Default = ImageBase 0x40us
end

[<AutoOpen>]
module ImageBase = let (|ImageBase|) (imageBase: ImageBase) = imageBase.Value

type ImageSubsystem =
    | Unknown = 0us
    | Native = 1us
    | WindowsGui = 2us
    | WindowsCui = 3us
    | OS2Cui = 5us
    | PosixCui = 7us
    | NativeWindows = 8us
    | WindowsCEGui = 9us
    | EfiApplication = 10us
    | EfiBootServiceDriver = 11us
    | EfiRuntimeDriver = 12us
    | EfiRom = 13us
    | Xbox = 14us
    | WindowsBootApplication = 16us

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

/// Represents the NT-specific fields of the optional header of a PE32 exectuable (II.25.2.3.2).
type NTSpecificFields<'ImageBase, 'Alignment, 'ImageSize, 'Size, 'NumDataDirectories> = // TODO: Don't make alignment a generic param.
    { ImageBase: 'ImageBase
      Alignment: 'Alignment
      OSMajor: uint16
      OSMinor: uint16
      UserMajor: uint16
      UserMinor: uint16
      SubSysMajor: uint16
      SubSysMinor: uint16
      /// Reserved value that must be set to zero.
      Win32VersionValue: uint32
      ImageSize: 'ImageSize
      HeadersSize: 'ImageSize
      FileChecksum: uint32
      Subsystem: ImageSubsystem
      DllFlags: PEFileFlags
      StackReserveSize: 'Size
      StackCommitSize: 'Size
      HeapReserveSize: 'Size
      HeapCommitSize: 'Size
      /// Reserved value that must be set to zero.
      LoaderFlags: uint32
      NumberOfDataDirectories: 'NumDataDirectories
      }

[<RequireQualifiedAccess>]
module NTSpecificFields = // TODO: Create a default fields module instead.
    let defaultFields =
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
          ImageSize = Omitted
          HeadersSize = Omitted
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
          LoaderFlags = 0u
          NumberOfDataDirectories = Omitted }
