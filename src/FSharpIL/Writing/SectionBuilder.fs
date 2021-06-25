namespace FSharpIL.Writing

open System.Collections.Immutable

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.PortableExecutable

[<Sealed>]
type SectionBuilder internal (alignment: Alignment, voffset: Rva, foffset: FileOffset) =
    let mutable section = ChunkedMemoryBuilder(int32 alignment.FileAlignment)
    member _.VirtualSize = section.Length
    member _.VirtualAddress = voffset
    member _.RawDataSize = Round.upTo alignment.FileAlignment section.Length
    member _.RawDataPointer = foffset

    member _.AddData(data: ImmutableArray<byte>) = section.Write data

    member this.AddData(metadata: CliMetadataBuilder) =
        let start = this.VirtualSize
        WriteCli.metadata &section this.VirtualAddress metadata
        { CliHeaderDirectory.Directory = { Rva = voffset + start; Size = this.VirtualSize - start }}

    member this.AddData(metadata: ModuleBuilder) = this.AddData(metadata.Serialize())

    member _.ToImmutable header = Section(header, section.ToImmutable())

/// Generates a section in a Portable Executable file.
type SectionContent = SectionBuilder -> DataDirectoriesBuilder -> struct(SectionName * SectionCharacteristics)
