namespace FSharpIL.PortableExecutable

open FSharpIL

type IPortableExecutable =
    abstract CoffHeader: CoffHeader<Omitted, Omitted>
    abstract OptionalHeader: OptionalHeader
    abstract Sections: System.Collections.Generic.IReadOnlyCollection<struct(SectionHeader * System.ReadOnlyMemory<byte>)>

// TODO: Make immutable version of PEFileBuilder
// type PEFile
