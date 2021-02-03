[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open System
open System.Collections.Generic

open FSharp.Core.Operators.Checked

open FSharpIL.PortableExecutable
open FSharpIL.Writing

[<RequireQualifiedAccess>]
module Size =
    let PEHeader = Magic.DosStub.Length + Magic.PESignature.Length |> uint32

    [<Literal>]
    let CoffHeader = 20u

    /// The value of the `SizeOfOptionalHeader` field in the COFF file header.
    [<Literal>]
    let OptionalHeader = 0xE0us

    [<Literal>]
    /// The size of the standard fields (PE32).
    let StandardFields = 28u

    [<Literal>]
    /// The size of the NT specific fields (PE32).
    let NTSpecificFields = 68u

    [<Literal>]
    /// The size of the ten data directories.
    let DataDirectories = 128u

    /// The length of a single section header.
    [<Literal>]
    let SectionHeader = 40u

type SectionInfo =
    { ActualSize: uint32
      FileOffset: uint32
      /// Specifies the RVA of the section.
      VirtualAddress: uint32
      Section: Section }

/// Stores the sizes and file offsets of the various objects and structs that make up a PE file.
[<ReferenceEquality; NoComparison>]
type PEInfo =
    { File: PEFile
      mutable CodeSize: uint32
      mutable InitializedDataSize: uint32
      mutable UninitializedDataSize: uint32
      mutable CliHeaderRva: uint32
      Sections: SectionInfo[] }

    // TODO: Make this a function.
    member this.RvaOf (section: (int * Section) option) =
        match section with
        | Some(i, _) -> (Array.get this.Sections i).VirtualAddress
        | None -> 0u

// II.25.2.2
let coffHeader pe (writer: ChunkWriter) =
    let coff = pe.FileHeader
    writer.WriteU2 coff.Machine
    writer.WriteU2 pe.SectionTable.Length
    writer.WriteU4 coff.TimeDateStamp
    writer.WriteU4 coff.SymbolTablePointer
    writer.WriteU4 coff.SymbolCount
    writer.WriteU2 Size.OptionalHeader
    writer.WriteU2 coff.Characteristics

// II.25.2.3.1
let standardFields info (writer: ChunkWriter) =
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
let ntSpecificFields pe (writer: ChunkWriter) =
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
let dataDirectories info (writer: ChunkWriter) =
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
    match info.File.CliHeader with
    | Some _ ->
        writer.WriteU4 info.CliHeaderRva
        writer.WriteU4 WriteCli.Size.CliHeader
    | None -> writer.WriteU8 0UL

    writer.WriteU8 0UL // Reserved

let sections (info: PEInfo) (writer: ChunkWriter) =
    let pe = info.File
    let falignment = uint32 pe.NTSpecificFields.FileAlignment
    let salignment = uint32 pe.NTSpecificFields.SectionAlignment

    let mutable fileOffset =
        Size.PEHeader
        + Size.CoffHeader
        + Size.StandardFields
        + Size.NTSpecificFields
        + Size.DataDirectories
        + (uint32 pe.SectionTable.Length * Size.SectionHeader)
        |> Round.upTo falignment

    let mutable virtualAddress = salignment

    let cliSectionIndex, cliDataIndex =
        match pe.DataDirectories.CliHeader with
        | Some header -> header.SectionIndex, header.DataIndex
        | None -> -1, -1

    for sectioni = 0 to pe.SectionTable.Length - 1 do
        let section = pe.SectionTable.Item sectioni
        for datai = 0 to section.Data.Length - 1 do
            match section.Data.Item datai with
            | CliHeader cli ->
                if sectioni = cliSectionIndex && datai = cliDataIndex then
                    info.CliHeaderRva <- virtualAddress
                WriteCli.metadata cli virtualAddress writer
            | ClrLoaderStub -> () // TODO: Write loader stub.
            | RawData bytes -> bytes() |> writer.WriteBytes

        let size = writer.Size
        writer.MoveToEnd() // Padding before next section.
        let roundedSize = writer.Size

        // Calculates the values for sizes in the standard fields (II.25.2.3.1)
        if section.Header.Characteristics.HasFlag SectionFlags.CntCode then
            info.CodeSize <- info.CodeSize + size
        if section.Header.Characteristics.HasFlag SectionFlags.CntInitializedData then
            info.InitializedDataSize <- info.InitializedDataSize + size
        if section.Header.Characteristics.HasFlag SectionFlags.CntUninitializedData then
            info.UninitializedDataSize <- info.UninitializedDataSize + size

        info.Sections.[sectioni] <-
            { ActualSize = size
              FileOffset = fileOffset
              VirtualAddress = virtualAddress
              Section = section }

        fileOffset <- fileOffset + roundedSize
        virtualAddress <- Round.upTo salignment (virtualAddress + size)

let write pe =
    try
        let info =
            { File = pe
              CodeSize = Unchecked.defaultof<uint32>
              InitializedDataSize = Unchecked.defaultof<uint32>
              UninitializedDataSize = Unchecked.defaultof<uint32>
              CliHeaderRva = Unchecked.defaultof<uint32>
              Sections = Array.zeroCreate<SectionInfo> pe.SectionTable.Length }
        let falignment = int32 pe.NTSpecificFields.FileAlignment
        let content = LinkedList<byte[]>()
        let headers = ChunkWriter(Array.zeroCreate falignment |> content.AddFirst)

        // Since information about sections is needed in the header, the
        // sections are written first before appending them after the headers.
        ChunkWriter(content.Last, falignment, falignment) |> sections info

        headers.WriteBytes Magic.DosStub
        headers.WriteBytes Magic.PESignature
        coffHeader pe headers
        standardFields info headers
        ntSpecificFields pe headers
        dataDirectories info headers

        // Section headers
        for i = 0 to info.File.SectionTable.Length - 1 do
            let section = Array.get info.Sections i
            let header = section.Section.Header
            SectionName.toArray header.SectionName |> headers.WriteBytes
            headers.WriteU4 section.ActualSize
            // TODO: Figure out how to calculate these fields from the data
            headers.WriteU4 section.VirtualAddress // VirtualAddress
            let roundedSize = Round.upTo (uint32 pe.NTSpecificFields.FileAlignment) section.ActualSize
            headers.WriteU4 roundedSize // SizeOfRawData
            headers.WriteU4 section.FileOffset // PointerToRawData
            headers.WriteU4 header.PointerToRelocations
            headers.WriteU4 0u // PointerToLineNumbers
            headers.WriteU2 header.NumberOfRelocations
            headers.WriteU2 0us // NumberOfLineNumbers
            headers.WriteU4 header.Characteristics

        content.Count * falignment, content
    with
    | ex -> InternalException ex |> raise

/// Builds a byte array containing the Portable Executable file.
let toArray pe =
    let size, content = write pe
    let output = Array.zeroCreate<byte> size
    let mutable offset = 0
    for chunk in content do
        let length = chunk.Length
        try
            Span(chunk).CopyTo(Span(output, offset, length))
            offset <- offset + length
        with
        | ex ->
            let msg =
                sprintf "Unable to copy chunk of length %i to offset %i." length offset
            InvalidOperationException(msg, ex) |> raise
    output

// TODO: Add toStream and toImmutableArray functions.
