namespace FSharpIL.PortableExecutable

open System.Collections.Immutable

open FSharpIL

/// Flags describing the section's characteristics (II.25.3).
[<System.Flags>]
type SectionCharacteristics =
    | CntCode = 0x20u
    | CntInitializedData = 0x40u
    | CntUninitializedData = 0x80u
    | MemDiscardable = 0x200_0000u
    | MemExecute = 0x2000_0000u
    | MemRead = 0x4000_0000u
    | MemWrite = 0x8000_0000u

/// (II.25.3)
type SectionHeader =
    { SectionName: SectionName
      VirtualSize: uint32
      VirtualAddress: uint32
      RawDataSize: uint32
      RawDataPointer: uint32
      /// Reserved value that should be set to zero.
      PointerToRelocations: uint32
      PointerToLineNumbers: uint32
      NumberOfRelocations: uint16
      NumberOfLineNumbers: uint16
      Characteristics: SectionCharacteristics }

// TODO: Have function for retrieving section header?
[<Sealed>]
type SectionBuilder internal (alignment: Alignment, name: SectionName, flags: SectionCharacteristics) =
    let data = ChunkedMemoryBuilder(int32 alignment.FileAlignment)
    member _.Name = name
    member _.VirtualSize = invalidOp "TODO: Get virtual size"
    member _.VirtualAddress = invalidOp "TODO: Get virtual address"
    member _.RawDataSize = invalidOp "TODO: Get raw data size from chunked builder"
    member _.RawDataPointer = invalidOp "TODO: Get raw data pointer"
    member _.Characteristics = flags
    member this.CreateHeader() =
        { SectionName = name
          VirtualSize = this.VirtualSize
          VirtualAddress = this.VirtualAddress
          RawDataSize = this.RawDataSize
          RawDataPointer = this.RawDataPointer
          PointerToRelocations = 0u
          PointerToLineNumbers = 0u
          NumberOfRelocations = 0us
          NumberOfLineNumbers = 0us
          Characteristics = flags }
