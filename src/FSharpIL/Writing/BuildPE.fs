[<RequireQualifiedAccess>]
module FSharpIL.Writing.BuildPE

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.PortableExecutable

open FSharpIL.Cli

open FSharpIL.Utilities
open FSharpIL.Utilities.Collections

[<IsByRefLike>]
[<NoComparison; NoEquality>]
type DataDirectoriesBuilder = struct
    [<DefaultValue>] val mutable ExportTable: RvaAndSize
    [<DefaultValue>] val mutable ImportTable: RvaAndSize
    [<DefaultValue>] val mutable ResourceTable: RvaAndSize
    [<DefaultValue>] val mutable ExceptionTable: RvaAndSize
    [<DefaultValue>] val mutable CertificateTable: RvaAndSize
    [<DefaultValue>] val mutable BaseRelocationTable: RvaAndSize
    [<DefaultValue>] val mutable DebugTable: RvaAndSize
    [<DefaultValue>] val mutable CopyrightTable: RvaAndSize
    [<DefaultValue>] val mutable GlobalPointerTable: RvaAndSize
    [<DefaultValue>] val mutable TLSTable: RvaAndSize
    [<DefaultValue>] val mutable LoadConfigTable: RvaAndSize
    [<DefaultValue>] val mutable BoundImportTable: RvaAndSize
    [<DefaultValue>] val mutable ImportAddressTable: RvaAndSize
    [<DefaultValue>] val mutable DelayImportDescriptor: RvaAndSize
    [<DefaultValue>] val mutable CliHeader: CliHeaderDirectory
    [<DefaultValue>] val mutable Reserved: RvaAndSize
end

[<IsByRefLike; Struct>]
[<NoComparison; NoEquality>]
type SectionBuilderState =
    { [<DefaultValue>] mutable DataDirectories: DataDirectoriesBuilder
      mutable SectionContent: ChunkedMemoryBuilder }

let rec writeSectionBuilder (state: byref<SectionBuilderState>) voffset foffset ustate (builder: SectionContentBuilder<'State>) =
    let size = state.SectionContent.Length
    match builder voffset foffset size ustate with
    | ValueSome(content, ustate') ->
        let start = size

        match content with
        | SectionContent.WriteContent writer ->
            writer.Invoke &state.SectionContent
        | SectionContent.SetCliHeader ->
            state.DataDirectories.CliHeader <-
                { CliHeaderDirectory.Directory = { Rva = voffset; Size = FSharpIL.Metadata.Magic.CliHeaderSize }}
        | SectionContent.WriteMetadata metadata ->
            WriteCli.metadata &state.SectionContent voffset metadata

        let length = state.SectionContent.Length - start
        let voffset' = voffset + length
        let foffset' = foffset + length

        writeSectionBuilder &state voffset' foffset' ustate' builder
    | ValueNone -> struct(size, ustate)

let ofSectionBuilders fileHeader (optionalHeader: OptionalHeader) (sections: ImmutableArray<SectionBuilder<'State>>) =
    let mutable sections' =
        if sections.IsDefaultOrEmpty
        then Array.empty
        else Array.zeroCreate<Section> sections.Length

    let alignment = optionalHeader.Alignment
    let fileHeadersSize = PEFile.calculateHeadersSize optionalHeader sections'.Length alignment.FileAlignment

    let mutable voffset = Rva(Round.upTo alignment.SectionAlignment fileHeadersSize)
    let mutable foffset = { FileOffset = fileHeadersSize }

    let mutable bstate = { SectionContent = ChunkedMemoryBuilder(int32 alignment.FileAlignment) }

    for i = 0 to sections'.Length - 1 do
        let (name, flags, ustate, builder) = sections.[i] voffset foffset

        let struct(size, ustate') = writeSectionBuilder &bstate voffset foffset ustate builder
        let content = bstate.SectionContent.MoveToImmutable()

        let header =
            { SectionName = name
              VirtualSize = size
              VirtualAddress = voffset
              RawDataSize = content.Length
              RawDataPointer = foffset
              PointerToRelocations = 0u
              PointerToLineNumbers = 0u
              NumberOfRelocations = 0us
              NumberOfLineNumbers = 0us
              Characteristics = flags }

        sections'.[i] <- Section(header, content)
        voffset <- voffset + Round.upTo alignment.SectionAlignment size
        foffset <- foffset + content.Length

    let directories =
        { ExportTable = bstate.DataDirectories.ExportTable
          ImportTable = bstate.DataDirectories.ImportTable
          ResourceTable = bstate.DataDirectories.ResourceTable
          ExceptionTable = bstate.DataDirectories.ExceptionTable
          CertificateTable = bstate.DataDirectories.CertificateTable
          BaseRelocationTable = bstate.DataDirectories.BaseRelocationTable
          DebugTable = bstate.DataDirectories.DebugTable
          CopyrightTable = bstate.DataDirectories.CopyrightTable
          GlobalPointerTable = bstate.DataDirectories.GlobalPointerTable
          TLSTable = bstate.DataDirectories.TLSTable
          LoadConfigTable = bstate.DataDirectories.LoadConfigTable
          BoundImportTable = bstate.DataDirectories.BoundImportTable
          ImportAddressTable = bstate.DataDirectories.ImportAddressTable
          DelayImportDescriptor = bstate.DataDirectories.DelayImportDescriptor
          CliHeader = bstate.DataDirectories.CliHeader
          Reserved = bstate.DataDirectories.Reserved }

    PEFile(fileHeader, optionalHeader, directories, Unsafe.As &sections', fileHeadersSize)

let ofMetadataBuilder flags builder =
    let text =
        SectionBuilder.ofList SectionName.text SectionCharacteristics.text [
            SectionContent.SetCliHeader
            SectionContent.WriteMetadata builder
        ]

    ofSectionBuilders
        { DefaultHeaders.coffHeader with Characteristics = flags }
        DefaultHeaders.optionalHeader
        (ImmutableArray.Create text)

let ofModuleBuilder flags (builder: CliModuleBuilder) = builder.Serialize() |> ofMetadataBuilder flags

let ofModule flags header root name mvid builder state =
    validated {
        let! metadata, state' = BuildCli.ModuleBuilder.run header root name mvid builder state
        return ofModuleBuilder flags metadata, state'
    }
