namespace FSharpIL.PortableExecutable // TODO: Might need to use functions from WriteCli module, so move to Writing namespace?

open System.Collections.Immutable

open FSharpIL

// TODO: Have function for retrieving section header?
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
