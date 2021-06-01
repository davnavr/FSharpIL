namespace FSharpIL.PortableExecutable

open System.Collections.Immutable

open FSharpIL

[<System.Runtime.CompilerServices.IsReadOnly>]
type Section = struct
    val Header: SectionHeader
    val Data: ChunkedMemory
    internal new (header, data) = { Header = header; Data = data }
end

[<Sealed>]
type PEFile internal
    (
        fileHeader: CoffHeader<Omitted, Omitted>,
        optionalHeader: OptionalHeader,
        dataDirectories: DataDirectories,
        sections: ImmutableArray<Section>
    )
    =
    member _.FileHeader = fileHeader
    member _.OptionalHeader = optionalHeader
    member _.DataDirectories = dataDirectories
    member _.Sections = sections
