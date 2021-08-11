namespace FSharpIL.Writing

open FSharpIL.PortableExecutable

type SectionContentWriter = delegate of byref<FSharpIL.ChunkedMemoryBuilder> -> unit

[<NoComparison; NoEquality>]
[<RequireQualifiedAccess>]
type SectionContent =
    | WriteContent of SectionContentWriter
    | WriteMetadata of CliMetadataBuilder
    | SetCliHeader

type SectionContentBuilder<'State> = Rva -> FileOffset -> uint32 -> 'State -> struct(SectionContent * 'State) voption

type SectionBuilder<'State> = Rva -> FileOffset -> SectionName * SectionCharacteristics * 'State * SectionContentBuilder<'State>

[<RequireQualifiedAccess>]
module SectionBuilder =
    let sectionListBuilder _ _ _ remaining =
        match remaining with
        | [] -> ValueNone
        | head :: tail -> ValueSome(struct(head, tail))

    let ofList name flags content: SectionBuilder<SectionContent list> = fun _ _ -> name, flags, content, sectionListBuilder
