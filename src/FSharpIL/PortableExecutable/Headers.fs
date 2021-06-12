namespace FSharpIL.PortableExecutable

open System
open System.Runtime.CompilerServices

open FSharpIL

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

/// Specifies the number of sections and defines various characteristics of the PE file (II.25.2.2).
type CoffHeader<'NumSections, 'HeaderSize> =
    { Machine: MachineFlags
      NumberOfSections: 'NumSections
      TimeDateStamp: uint32
      SymbolTablePointer: uint32
      SymbolCount: uint32
      OptionalHeaderSize: 'HeaderSize
      Characteristics: ImageFileFlags }

/// <summary>Represents the value of the <c>Magic</c> field in the PE file standard fields.</summary>
type ImageKind =
    /// The file is a ROM image.
    | ROM = 0x107us
    /// The file is a normal PE32 executable.
    | PE32 = 0x10Bus
    /// The file is a PE32+ executable.
    | PE32Plus = 0x20Bus

/// (II.25.2.3.1)
type StandardFields<'Magic, 'Size, 'BaseOfData> =
    { Magic: 'Magic
      LMajor: byte
      LMinor: byte
      CodeSize: 'Size
      InitializedDataSize: 'Size
      UninitializedDataSize: 'Size
      EntryPointRva: 'Size
      BaseOfCode: 'Size
      // Note that this field is absent in PE32+
      BaseOfData: 'BaseOfData }

[<IsReadOnly; Struct>]
type ImageBase = struct
    val internal Value: uint16
    new (value) = { Value = value }
    static member op_Implicit(imageBase: ImageBase) = uint32 imageBase.Value * 0x10000u
    static member inline op_Implicit(imageBase: ImageBase) = uint64(uint32 imageBase)
    static member Default = ImageBase 0x40us
end

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
type NTSpecificFields<'ImageBase, 'ImageSize, 'Size, 'NumDataDirectories> =
    { ImageBase: 'ImageBase
      Alignment: Alignment
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
      NumberOfDataDirectories: 'NumDataDirectories }

/// Represents the optional header of the PE file which, despite the name, is not optional (II.25.2.3).
type OptionalHeader<'ImageKind, 'StandardField, 'ImageBase, 'ImageBasePlus, 'ImageSize, 'NumDataDirectories> =
    | PE32 of
        StandardFields<'ImageKind, 'StandardField, uint32> *
        NTSpecificFields<'ImageBase, 'ImageSize, uint32, 'NumDataDirectories>
    | PE32Plus of
        StandardFields<'ImageKind, 'StandardField, Omitted> *
        NTSpecificFields<'ImageBasePlus, 'ImageSize, uint64, 'NumDataDirectories>

    member inline this.Alignment =
        match this with
        | PE32(_, { Alignment = alignment })
        | PE32Plus(_, { Alignment = alignment }) -> alignment

    member inline this.NumberOfDataDirectories =
        match this with
        | PE32(_, { NumberOfDataDirectories = count })
        | PE32Plus(_, { NumberOfDataDirectories = count }) -> count

// TODO: Better alias for optional header used in building PE files.
type OptionalHeader = OptionalHeader<Omitted, Omitted, ImageBase, ImageBase, Omitted, Omitted>
