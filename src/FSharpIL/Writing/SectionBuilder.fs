namespace FSharpIL.PortableExecutable // TODO: Might need to use functions from WriteCli module, so move to Writing namespace?

open System.Collections.Immutable

open FSharpIL

// TODO: To allow easy tracking of RVAs prevent modification of previous sections by ensuring PEFileBuilder only accepts Section instances?
[<Sealed>]
type SectionBuilder internal (alignment: Alignment, name: SectionName, flags: SectionCharacteristics) =
    let mutable section = ChunkedMemoryBuilder(int32 alignment.FileAlignment)
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
    member _.AddData(data: ImmutableArray<byte>) = section.Write data
    //member this.AddCliMetadata(metadata: CliMetadata): CliHeaderDirectory =
    //    
    // NOTE: Changes to the underlying chunks would result in changes in the "immutable" version, but all Add methods so far do not modify existing data.
    member this.ToImmutable() = Section(this.CreateHeader(), section.ToImmutable())
