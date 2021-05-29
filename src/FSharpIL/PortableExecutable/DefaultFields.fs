[<RequireQualifiedAccess>]
module FSharpIL.PortableExecutable.DefaultFields

open FSharpIL

/// <summary>Default PE file header indicating that the file is a <c>.dll</c> file.</summary>
let coffHeader =
    { Machine = MachineFlags.I386
      NumberOfSections = Omitted
      TimeDateStamp = 0u
      SymbolTablePointer = 0u
      SymbolCount = 0u
      OptionalHeaderSize = Omitted
      Characteristics = ImageFileFlags.dll }

let standardFields =
    { Magic = Omitted
      LMajor = 8uy
      LMinor = 0uy
      CodeSize = Omitted
      InitializedDataSize = Omitted
      UninitializedDataSize = Omitted
      EntryPointRva = Omitted
      BaseOfCode = Omitted
      BaseOfData = Omitted }

let ntSpecificFields =
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
