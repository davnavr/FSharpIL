namespace FSharpIL.Writing

open FSharpIL.PortableExecutable

type SectionContentWriter = delegate of byref<FSharpIL.ChunkedMemoryBuilder> -> unit

[<NoComparison; NoEquality>]
[<RequireQualifiedAccess>]
type SectionContent =
    | WriteContent of SectionContentWriter
    | WriteMetadata of CliMetadataBuilder
    /// Sets the RVA and size of the CLI header data directory.
    | SetCliHeader
    //| SetDataDirectory of DataDirectory * RvaAndSize

type SectionContentBuilder<'State> = Rva -> FileOffset -> uint32 -> 'State -> struct(SectionContent * 'State) voption

/// Creates a section in a Portable Executable file.
type SectionBuilder<'State> = Rva -> FileOffset -> SectionName * SectionCharacteristics * 'State * SectionContentBuilder<'State>

[<RequireQualifiedAccess>]
module SectionBuilder =
    val ofList: name: SectionName -> flags: SectionCharacteristics -> content: SectionContent list -> SectionBuilder<SectionContent list>
