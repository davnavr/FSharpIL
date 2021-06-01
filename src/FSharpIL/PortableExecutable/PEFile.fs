namespace FSharpIL.PortableExecutable

open System.Collections.Generic

open FSharpIL

[<System.Runtime.CompilerServices.IsReadOnly>]
type Section = struct
    val Header: SectionHeader
    val Data: IReadOnlyCollection<System.ReadOnlyMemory<byte>>
    internal new (header, data) = { Header = header; Data = data }
end

type IPortableExecutable =
    abstract CoffHeader: CoffHeader<Omitted, Omitted>
    abstract OptionalHeader: OptionalHeader
    abstract DataDirectories: DataDirectories
    abstract Sections: IReadOnlyCollection<Section>

// TODO: Make immutable version of PEFileBuilder
// type PEFile
