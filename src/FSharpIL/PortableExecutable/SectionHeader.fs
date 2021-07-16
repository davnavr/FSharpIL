namespace FSharpIL.PortableExecutable

open FSharpIL.Utilities.Compare

/// Flags describing the characteristics of a PE file section (II.25.3).
[<System.Flags>]
type SectionCharacteristics =
    | TypeNoPad = 0x8u
    /// The section contains executable code.
    | CntCode = 0x20u
    /// The section contains initialized data.
    | CntInitializedData = 0x40u
    /// The section contains uninitialized data.
    | CntUninitializedData = 0x80u
    | LnkOther = 0x100u
    | LnkInfo = 0x200u
    | LnkRemove = 0x800u
    | LnkComDat = 0x1000u
    | GpRel = 0x8000u
    | LnkNRelocOvfl = 0x100_0000u
    | MemDiscardable = 0x200_0000u
    | MemNotCached = 0x400_0000u
    | MemNotPaged = 0x800_0000u
    | MemShared = 0x1000_0000u
    /// The section can be executed as code.
    | MemExecute = 0x2000_0000u
    /// The section can be read.
    | MemRead = 0x4000_0000u
    /// The section can be written to.
    | MemWrite = 0x8000_0000u

/// Describes the location, size, and characteristics of a section (II.25.3).
type SectionHeader =
    { SectionName: SectionName
      VirtualSize: uint32
      VirtualAddress: Rva
      RawDataSize: uint32
      RawDataPointer: FileOffset
      /// Reserved value that should be set to zero.
      PointerToRelocations: uint32
      PointerToLineNumbers: uint32
      NumberOfRelocations: uint16
      NumberOfLineNumbers: uint16
      Characteristics: SectionCharacteristics }

[<RequireQualifiedAccess>]
module SectionCharacteristics =
    let text = SectionCharacteristics.CntCode ||| SectionCharacteristics.MemExecute ||| SectionCharacteristics.MemRead

[<RequireQualifiedAccess>]
module SectionHeader =
    /// Determines whether the specified Relative Virtual Address is contain within the specfied section.
    let contains (rva: Rva) header = rva .>= header.VirtualAddress && rva <. (header.VirtualAddress + header.VirtualSize)
