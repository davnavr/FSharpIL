[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open FSharp.Core.Operators.Checked

open FSharpIL.Bytes
open FSharpIL.PortableExecutable

[<RequireQualifiedAccess>]
module Size =
    let PEHeader = Magic.DosStub.Length + Magic.PESignature.Length |> uint64

    [<Literal>]
    let CoffHeader = 20UL

    /// The value of the `SizeOfOptionalHeader` field in the COFF file header.
    [<Literal>]
    let OptionalHeader = 0xE0us

    [<Literal>]
    /// The size of the standard fields (PE32).
    let StandardFields = 28UL

    [<Literal>]
    /// The size of the NT specific fields (PE32).
    let NTSpecificFields = 68UL

    [<Literal>]
    /// The size of the ten data directories.
    let DataDirectories = 128UL

    /// The length of a single section header.
    [<Literal>]
    let SectionHeader = 40UL

type SectionInfo =
    { ActualSize: uint64
      FileOffset: uint64
      RawData: byte[]
      Section: Section }

/// Stores the sizes and file offsets of the various objects and structs that make up a PE file.
[<Sealed>]
type PEInfo (pe: PEFile) =
    let mutable codeSize = 0UL
    let mutable initializedDataSize = 0UL
    let mutable uninitializedDataSize = 0UL
    let headersSize =
        Size.PEHeader
        + Size.CoffHeader
        + Size.StandardFields
        + Size.NTSpecificFields
        + Size.DataDirectories
        + (uint64 pe.SectionTable.Length * Size.SectionHeader)
        |> Round.upTo (uint64 pe.NTSpecificFields.FileAlignment)
    let sections = Array.zeroCreate<SectionInfo> pe.SectionTable.Length
    let mutable cliHeaderRva = 0UL

    do
        let cliSectionIndex, cliDataIndex =
            match pe.DataDirectories.CliHeader with
            | Some header -> header.SectionIndex, header.DataIndex
            | None -> -1, -1

        for sectionIndex = 0 to pe.SectionTable.Length - 1 do
            let section = pe.SectionTable.Item sectionIndex

            let fileOffset =
                if sectionIndex = 0
                then headersSize
                else
                    let prevSection = Array.get sections (sectionIndex - 1)
                    prevSection.FileOffset + uint64 prevSection.RawData.Length

            let writer = int32 pe.NTSpecificFields.FileAlignment |> ResizeByteWriter
            for dataIndex = 0 to section.Data.Length - 1 do
                match section.Data.Item dataIndex with
                | CliHeader cli ->
                    if sectionIndex = cliSectionIndex && dataIndex = cliDataIndex then
                        cliHeaderRva <- fileOffset + writer.Position
                    // TODO: Write CLI data.
                    ()
                | ClrLoaderStub -> () // TODO: Write the loader stub.
                | RawData(Lazy data) ->
                    writer.WriteBytes data
            let actualSize = writer.Position

            let data = writer.UnderlyingArray()

            // Calculates the values for sizes in the standard fields (II.25.2.3.1)
            let len = uint64 data.Length
            if section.Header.Characteristics.HasFlag SectionFlags.CntCode then
                codeSize <- codeSize + len
            if section.Header.Characteristics.HasFlag SectionFlags.CntInitializedData then
                initializedDataSize <- initializedDataSize + len
            if section.Header.Characteristics.HasFlag SectionFlags.CntUninitializedData then
                uninitializedDataSize <- uninitializedDataSize + len

            sections.[sectionIndex] <-
                { ActualSize = actualSize
                  FileOffset = fileOffset
                  RawData = data
                  Section = section }

    member _.File = pe

    member _.CodeSize = codeSize
    member _.InitializedDataSize = initializedDataSize
    member _.UninitializedDataSize = uninitializedDataSize

    member _.HeadersSize = headersSize

    member _.Sections = sections

    member val CliHeaderRva = cliHeaderRva

    /// Calculates the RVA of the specified section.
    member _.RvaOf (section: (int * Section) option) =
        match section with
        | Some(i, _) -> (Array.get sections i).FileOffset
        | None -> 0UL

// II.25.2.2
let coffHeader (pe, writer: ByteWriter) =
    let coff = pe.FileHeader
    writer.WriteU2 coff.Machine
    writer.WriteU2 pe.SectionTable.Length
    writer.WriteU4 coff.TimeDateStamp
    writer.WriteU4 coff.SymbolTablePointer
    writer.WriteU4 coff.SymbolCount
    writer.WriteU2 Size.OptionalHeader
    writer.WriteU2 coff.Characteristics

// II.25.2.3.1
let standardFields (info: PEInfo, writer: ByteWriter) =
    let pe = info.File
    let standard = info.File.StandardFields
    writer.WriteU2 Magic.PE32
    writer.WriteU1 standard.LMajor
    writer.WriteU1 standard.LMinor
    writer.WriteU4 info.CodeSize
    writer.WriteU4 info.InitializedDataSize
    writer.WriteU4 info.UninitializedDataSize
    // NOTE: The EntryPointRva always has a value regardless of whether or not it is a .dll or .exe, and points to somewhere special (see the end of II.25.2.3.1)
    writer.WriteU4 0u // EntryPointRva // TODO: Figure out what this value should be.
    info.RvaOf pe.Sections.TextSection |> writer.WriteU4 // BaseOfCode, matches the RVA of the .text section
    info.RvaOf pe.Sections.RsrcSection |> writer.WriteU4 // BaseOfData, matches the RVA of the .rsrc section

// II.25.2.3.2
let ntSpecificFields (pe: PEFile, writer: ByteWriter) =
    let nt = pe.NTSpecificFields
    writer.WriteU4 nt.ImageBase
    writer.WriteU2 nt.SectionAlignment
    writer.WriteU2 nt.FileAlignment
    writer.WriteU2 nt.OSMajor
    writer.WriteU2 nt.OSMinor
    writer.WriteU2 nt.UserMajor
    writer.WriteU2 nt.UserMinor
    writer.WriteU2 nt.SubSysMajor
    writer.WriteU2 nt.SubSysMinor
    writer.WriteU4 nt.Win32VersionValue
    writer.WriteU4 0u // ImageSize // TODO: Figure out how to calculate the ImageSize
    writer.WriteU4 pe.NTSpecificFields.FileAlignment
    writer.WriteU4 nt.FileChecksum
    writer.WriteU4 nt.Subsystem
    writer.WriteU4 nt.DllFlags
    writer.WriteU4 nt.StackReserveSize
    writer.WriteU4 nt.StackCommitSize
    writer.WriteU4 nt.HeapReserveSize
    writer.WriteU4 nt.HeapCommitSize
    writer.WriteU4 nt.LoaderFlags
    writer.WriteU4 0x10u // NumberOfDataDirectories

// II.25.2.3.3
let dataDirectories (info: PEInfo, writer: ByteWriter) =
    writer.WriteU8 0UL // ExportTable
    writer.WriteU8 0UL // TEMPORARY // ImportTable
    writer.WriteU8 0UL // ResourceTable
    writer.WriteU8 0UL // ExceptionTable
    writer.WriteU8 0UL // CertificateTable
    writer.WriteU8 0UL // TEMPORARY // BaseRelocationTable
    writer.WriteU8 0UL // DebugTable
    writer.WriteU8 0UL // CopyrightTable
    writer.WriteU8 0UL // GlobalPointerTable
    writer.WriteU8 0UL // TLSTable
    writer.WriteU8 0UL // LoadConfigTable
    writer.WriteU8 0UL // BoundImportTable
    writer.WriteU8 0UL // TEMPORARY // ImportAddressTable
    writer.WriteU8 0UL // DelayImportDescriptor

    // CliHeader
    if info.File.CliHeader.IsSome then
        writer.WriteU4 info.CliHeaderRva
        writer.WriteU4 WriteCli.Size.CliHeader
    else writer.WriteU8 0UL

    writer.WriteU8 0UL // Reserved

let headers (info: PEInfo, writer: ChunkedByteWriter) =
    coffHeader(info.File, writer)
    standardFields(info, writer)
    ntSpecificFields(info.File, writer)
    dataDirectories(info, writer)

    for i = 0 to info.File.SectionTable.Length - 1 do
        let section = Array.get info.Sections i
        let header = section.Section.Header
        SectionName.toArray header.SectionName |> writer.WriteBytes
        writer.WriteU4 section.ActualSize
        // TODO: Figure out how to calculate these fields from the data
        writer.WriteU4 section.FileOffset // VirtualAddress
        writer.WriteU4 section.RawData.Length
        writer.WriteU4 section.FileOffset // PointerToRawData
        writer.WriteU4 header.PointerToRelocations
        writer.WriteU4 0u // PointerToLineNumbers
        writer.WriteU2 header.NumberOfRelocations
        writer.WriteU2 0us // NumberOfLineNumbers
        writer.WriteU4 header.Characteristics

    writer.NextChunk()

let write (pe: PEFile) =
    try
        let falignment = pe.NTSpecificFields.FileAlignment
        let info = PEInfo pe
        let chunks =
            int32 (info.HeadersSize / uint64 falignment)
            + Array.sumBy
                (fun section -> section.RawData.Length)
                info.Sections
        let writer = ChunkedByteWriter(chunks, int32 falignment)

        writer.WriteBytes Magic.DosStub
        writer.WriteBytes Magic.PESignature

        headers(info, writer)

        for section in info.Sections do writer.WriteBytes section.RawData

        writer
    with
    | ex -> InternalException ex |> raise

let toArray pe = write(pe).ToArray()

// TODO: Add toSpan, toStream and toImmutableArray functions.
