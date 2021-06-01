namespace FSharpIL.PortableExecutable

open FSharpIL

type IPortableExecutable =
    abstract CoffHeader: CoffHeader<Omitted, Omitted>
    abstract OptionalHeader: OptionalHeader
    abstract Sections: seq<struct(SectionHeader * ChunkedMemory)>

// TODO: Make immutable version of PEFileBuilder
// type PEFile
