[<RequireQualifiedAccess>]
module FSharpIL.Writing.BuildPE

open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Utilities

open FSharpIL.PortableExecutable

let create fileHeader (optionalHeader: OptionalHeader) (sections: ImmutableArray<_>) =
    let mutable sections' =
        if sections.IsDefaultOrEmpty
        then Array.empty
        else Array.zeroCreate<Section> sections.Length

    let alignment = optionalHeader.Alignment
    let dataDirectories = DataDirectoriesBuilder()
    let fileHeadersSize = PEFile.calculateHeadersSize optionalHeader sections'.Length alignment.FileAlignment

    let mutable voffset = Rva(Round.upTo alignment.SectionAlignment fileHeadersSize)
    let mutable foffset = { FileOffset = fileHeadersSize }
    for i = 0 to sections'.Length do
        let builder = SectionBuilder(alignment, voffset, foffset)
        let struct(name, flags) = sections.[i] builder dataDirectories

        sections'.[i] <-
            builder.ToImmutable
                { SectionName = name
                  VirtualSize = builder.VirtualSize
                  VirtualAddress = builder.VirtualAddress
                  RawDataSize = builder.RawDataSize
                  RawDataPointer = builder.RawDataPointer
                  PointerToRelocations = 0u
                  PointerToLineNumbers = 0u
                  NumberOfRelocations = 0us
                  NumberOfLineNumbers = 0us
                  Characteristics = flags }

        voffset <- voffset + Rva(Round.upTo alignment.SectionAlignment builder.VirtualSize)
        foffset <- foffset + builder.RawDataSize

    PEFile(fileHeader, optionalHeader, dataDirectories.ToImmutable(), Unsafe.As &sections', fileHeadersSize)
