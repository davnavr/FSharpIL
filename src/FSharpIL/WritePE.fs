[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO

open Microsoft.FSharp.Core.Operators.Checked

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

    /// The length of a single section header.
    [<Literal>]
    let SectionHeader = 40u

[<ReferenceEquality; NoComparison>]
type PEInfo =
    { File: PEFile
      mutable CodeSize: ChunkWriter
      mutable InitializedDataSize: ChunkWriter
      mutable UninitializedDataSize: ChunkWriter
      mutable BaseOfCode: ChunkWriter
      mutable BaseOfData: ChunkWriter
      mutable CliHeaderRva: ChunkWriter
      Sections: ChunkWriter[] }

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

    info.CodeSize <- writer.CreateWriter()
    writer.SkipBytes 4u

    info.InitializedDataSize <- writer.CreateWriter()
    writer.SkipBytes 4u

    info.UninitializedDataSize <- writer.CreateWriter()
    writer.SkipBytes 4u

    // NOTE: The EntryPointRva always has a value regardless of whether or not it is a .dll or .exe, and points to somewhere special (see the end of II.25.2.3.1)
    writer.WriteU4 0u // EntryPointRva // TODO: Figure out what this value should be.

    info.BaseOfCode <- writer.CreateWriter() // RVA of the .text section
    writer.SkipBytes 4u

    info.BaseOfData <- writer.CreateWriter() // RVA of the .rsrc section
    writer.SkipBytes 4u

// II.25.2.3.2
let ntSpecificFields pe (writer: ChunkWriter) =
    let nt = pe.NTSpecificFields
    writer.WriteU4 nt.ImageBase
    writer.WriteU4 nt.SectionAlignment
    writer.WriteU4 nt.FileAlignment
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
    writer.WriteU2 nt.Subsystem
    writer.WriteU2 nt.DllFlags
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
        info.CliHeaderRva <- writer.CreateWriter()
        writer.SkipBytes 4u
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
        + uint32 Size.OptionalHeader
        + (uint32 pe.SectionTable.Length * Size.SectionHeader)
        |> Round.upTo falignment

    let mutable virtualAddress = salignment

    let mutable codeSize, initializedDataSize, uninitializedDataSize = 0u, 0u, 0u

    let cliSectionIndex, cliDataIndex =
        match pe.DataDirectories.CliHeader with
        | Some header -> header.SectionIndex, header.DataIndex
        | None -> -1, -1

    for sectioni = 0 to pe.SectionTable.Length - 1 do
        writer.ResetSize()
        let section = pe.SectionTable.Item sectioni
        for datai = 0 to section.Data.Length - 1 do
            match section.Data.Item datai with
            | CliHeader cli ->
                if sectioni = cliSectionIndex && datai = cliDataIndex then
                    info.CliHeaderRva.WriteU4 virtualAddress
                WriteCli.metadata cli virtualAddress writer
            | ClrLoaderStub -> () // TODO: Write loader stub.
            | RawData bytes -> bytes() |> writer.WriteBytes

        let size = writer.Size
        writer.MoveToEnd() // Padding before next section.
        let roundedSize = writer.Size

        // Calculates the values for sizes in the standard fields (II.25.2.3.1)
        if section.Header.Characteristics.HasFlag SectionFlags.CntCode then
            codeSize <- codeSize + size
        if section.Header.Characteristics.HasFlag SectionFlags.CntInitializedData then
            initializedDataSize <- initializedDataSize + size
        if section.Header.Characteristics.HasFlag SectionFlags.CntUninitializedData then
            uninitializedDataSize <- uninitializedDataSize + size

        let header = info.Sections.[sectioni]
        header.WriteU4 size // VirtualSize, actual size of the section
        header.WriteU4 virtualAddress // VirtualAddress
        header.WriteU4 roundedSize // SizeOfRawData, size of section rounded up to file alignment
        header.WriteU4 fileOffset // PointerToRawData

        fileOffset <- fileOffset + roundedSize
        virtualAddress <- Round.upTo salignment (virtualAddress + size)

    info.CodeSize.WriteU4 codeSize
    info.InitializedDataSize.WriteU4 codeSize
    info.UninitializedDataSize.WriteU4 codeSize

let write pe =
    try
        let info =
            let uninitialized = Unchecked.defaultof<ChunkWriter>
            { File = pe
              CodeSize = uninitialized
              InitializedDataSize = uninitialized
              UninitializedDataSize = uninitialized
              BaseOfCode = uninitialized
              BaseOfData = uninitialized
              CliHeaderRva = uninitialized
              Sections = Array.zeroCreate pe.SectionTable.Length }
        let falignment = int32 pe.NTSpecificFields.FileAlignment
        let content = LinkedList<byte[]>()
        let writer = ChunkWriter(Array.zeroCreate falignment |> content.AddFirst)

        writer.WriteBytes Magic.DosStub
        writer.WriteBytes Magic.PESignature
        coffHeader pe writer
        standardFields info writer
        ntSpecificFields pe writer
        dataDirectories info writer

        // Section headers
        for i = 0 to info.File.SectionTable.Length - 1 do
            let section = pe.SectionTable.Item i
            let header = section.Header
            SectionName.toArray header.SectionName |> writer.WriteBytes
            // VirtualSize, VirtualAddress, SizeOfRawData, PointerToRawData
            let header' = writer.CreateWriter()
            writer.SkipBytes 16u
            writer.WriteU4 header.PointerToRelocations
            writer.WriteU4 0u // PointerToLineNumbers
            writer.WriteU2 header.NumberOfRelocations
            writer.WriteU2 0us // NumberOfLineNumbers
            writer.WriteU4 header.Characteristics
            info.Sections.[i] <- header'

        writer.MoveToEnd() // Padding before sections

        sections info writer

        content.Count * falignment, content
    with
    | ex -> InternalException ex |> raise

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

let toBlock pe =
    let size, content = write pe
    let builder = ImmutableArray.CreateBuilder<byte> size
    for chunk in content do
        builder.AddRange chunk
    builder.ToImmutable()

let stream pe =
    // TODO: Return an optimized stream type instead.
    new MemoryStream(toArray pe) :> Stream

// TODO: Add toSeq function.

let toStream pe (stream: Stream) =
    match stream with
    | null -> nameof stream |> nullArg
    | _ when not stream.CanWrite -> invalidArg (nameof stream) "The stream must support writing"
    | _ ->
        try
            let _, content = write pe
            for chunk in content do
                stream.Write(ReadOnlySpan chunk)
        finally
            stream.Close()

let toFile pe =
    function
    | null -> nullArg "file"
    | (file: FileInfo) -> file.OpenWrite() |> toStream pe

let toPath pe =
    function
    | null -> nullArg "path"
    | path -> File.OpenWrite path |> toStream pe
