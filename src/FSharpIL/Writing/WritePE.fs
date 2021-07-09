[<RequireQualifiedAccess>]
module FSharpIL.Writing.WritePE

open System
open System.Collections.Immutable
open System.IO

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.PortableExecutable

/// Contains information about the Portable Executable file.
[<NoEquality; NoComparison>]
type PEInfo =
    { FileHeader: CoffHeader<Omitted, Omitted>
      OptionalHeader: OptionalHeader
      DataDirectories: DataDirectories
      CodeSize: uint32
      InitializedDataSize: uint32
      UninitializedDataSize: uint32
      BaseOfCode: Rva
      BaseOfData: Rva
      FileAlignment: uint32
      SectionAlignment: uint32
      ImageSize: uint32
      HeadersSize: uint32
      Sections: ImmutableArray<Section>
      CliHeaderRva: Rva }

let getFileInfo (file: PEFile) =
    let optionalHeader, dataDirectories, sections = file.OptionalHeader, file.DataDirectories, file.Sections
    let falignment, salignment =
        uint32 optionalHeader.Alignment.FileAlignment, uint32 optionalHeader.Alignment.SectionAlignment

    let mutable codeSize, initDataSize, uninitDataSize, imageSize = 0u, 0u, 0u, Round.upTo salignment file.SizeOfHeaders
    let mutable baseOfCode, baseOfData, rva = Rva.Zero, Rva.Zero, Rva salignment

    for section in sections do
        let flags, size = section.Header.Characteristics, section.Data.Length
        if flags.HasFlag SectionCharacteristics.CntCode then codeSize <- codeSize + size
        if flags.HasFlag SectionCharacteristics.CntInitializedData then initDataSize <- initDataSize + size
        if flags.HasFlag SectionCharacteristics.CntUninitializedData then uninitDataSize <- uninitDataSize + size

        if baseOfCode = Rva.Zero && section.Header.SectionName = SectionName.text then baseOfCode <- rva
        if baseOfData = Rva.Zero && section.Header.SectionName = SectionName.rsrc then baseOfData <- rva

        imageSize <- imageSize + (Round.upTo salignment size)
        rva <- rva + Round.upTo salignment size

    { FileHeader = file.FileHeader
      OptionalHeader = file.OptionalHeader
      DataDirectories = dataDirectories
      CodeSize = codeSize
      InitializedDataSize = initDataSize
      UninitializedDataSize = uninitDataSize
      BaseOfCode = baseOfCode
      BaseOfData = baseOfData
      FileAlignment = falignment
      SectionAlignment = salignment
      ImageSize = imageSize
      HeadersSize = file.SizeOfHeaders
      Sections = file.Sections
      CliHeaderRva = dataDirectories.CliHeader.Directory.Rva }

[<Struct>]
type FileHeaderWriter<'Writer when 'Writer :> IByteWriter> = struct
    val mutable private output: 'Writer
    val mutable private pos: uint32

    new (output) = { output = output; pos = 0u }

    member this.Output = this.output
    member this.Position = this.pos

    member this.Write data =
        this.output.Write data
        this.pos <- this.pos + uint32 data.Length

    member this.AlignTo alignment =
        let mutable padding = (Round.upTo alignment this.Position) - this.Position
        let buffer = Span.stackalloc<byte> 512

        buffer.Clear()

        while padding > 0u do
            let length =
                if padding > uint32 buffer.Length
                then buffer.Length
                else int32 padding
            this.Write(buffer.Slice(0, length))
            padding <- padding - uint32 length

    interface IByteWriter with member this.Write data = this.Write data
end

/// Writes the PE file header (II.25.2.2).
let coffHeader info (writer: byref<#IByteWriter>) =
    let coff = info.FileHeader
    writer.WriteLE(uint16 coff.Machine)
    writer.WriteLE(uint16 info.Sections.Length)
    writer.WriteLE coff.TimeDateStamp
    writer.WriteLE coff.SymbolTablePointer
    writer.WriteLE coff.SymbolCount
    writer.WriteLE Magic.OptionalHeaderSize
    writer.WriteLE(uint16 coff.Characteristics)

let standardFieldsCommon (imageKind: ImageKind) fields info (writer: byref<#IByteWriter>) =
    writer.WriteLE(uint16 imageKind)
    let lversion = Span.stackalloc<byte> 2
    lversion.[0] <- fields.LMajor
    lversion.[1] <- fields.LMinor
    writer.Write lversion
    writer.WriteLE info.CodeSize
    writer.WriteLE info.InitializedDataSize
    writer.WriteLE info.UninitializedDataSize
    // NOTE: The EntryPointRva always has a value regardless of whether or not it is a .dll or .exe, and points to somewhere special (see the end of II.25.2.3.1)
    writer.WriteLE 0u // EntryPointRva // TODO: Figure out what this value should be.
    writer.WriteLE(uint32 info.BaseOfCode) // RVA of the .text section

let ntSpecificFieldsCommon (fields: NTSpecificFields<_, _, _, _>) info (writer: byref<#IByteWriter>) =
    writer.WriteLE(uint32 fields.Alignment.SectionAlignment)
    writer.WriteLE(uint32 fields.Alignment.FileAlignment)
    writer.WriteLE fields.OSMajor
    writer.WriteLE fields.OSMinor
    writer.WriteLE fields.UserMajor
    writer.WriteLE fields.UserMinor
    writer.WriteLE fields.SubSysMajor
    writer.WriteLE fields.SubSysMinor
    writer.WriteLE fields.Win32VersionValue
    writer.WriteLE info.ImageSize
    writer.WriteLE info.HeadersSize
    writer.WriteLE fields.FileChecksum
    writer.WriteLE(uint16 fields.Subsystem)
    writer.WriteLE(uint16 fields.DllFlags)

let inline dataDirectory (dir: inref<RvaAndSize>) (writer: byref<#IByteWriter>) =
    writer.WriteLE(uint32 dir.Rva)
    writer.WriteLE dir.Size

/// Writes the PE optional header (II.25.2.3).
let optionalHeader ({ DataDirectories = directories } as info) (writer: byref<#IByteWriter>) =
    match info.OptionalHeader with
    | PE32(standard, nt) ->
        standardFieldsCommon ImageKind.PE32 standard info &writer
        writer.WriteLE(uint32 info.BaseOfData) // RVA of the .rsrc section

        writer.WriteLE(uint32 nt.ImageBase)
        ntSpecificFieldsCommon nt info &writer
        writer.WriteLE nt.StackReserveSize
        writer.WriteLE nt.StackCommitSize
        writer.WriteLE nt.HeapReserveSize
        writer.WriteLE nt.HeapCommitSize
        writer.WriteLE nt.LoaderFlags
    | PE32Plus(standard, nt) ->
        standardFieldsCommon ImageKind.PE32Plus standard info &writer

        writer.WriteLE(uint64 nt.ImageBase)
        ntSpecificFieldsCommon nt info &writer
        writer.WriteLE(uint64 nt.StackReserveSize)
        writer.WriteLE(uint64 nt.StackCommitSize)
        writer.WriteLE(uint64 nt.HeapReserveSize)
        writer.WriteLE(uint64 nt.HeapCommitSize)
        // NOTE: Duplicate code for LoaderFlags.
        writer.WriteLE nt.LoaderFlags
    writer.WriteLE 0x10u // NumberOfRvaAndSizes
    dataDirectory &directories.ExportTable &writer
    dataDirectory &directories.ImportTable &writer
    dataDirectory &directories.ResourceTable &writer
    dataDirectory &directories.ExceptionTable &writer
    dataDirectory &directories.CertificateTable &writer
    dataDirectory &directories.BaseRelocationTable &writer
    dataDirectory &directories.DebugTable &writer
    dataDirectory &directories.CopyrightTable &writer
    dataDirectory &directories.GlobalPointerTable &writer
    dataDirectory &directories.TLSTable &writer
    dataDirectory &directories.LoadConfigTable &writer
    dataDirectory &directories.BoundImportTable &writer
    dataDirectory &directories.ImportAddressTable &writer
    dataDirectory &directories.DelayImportDescriptor &writer
    dataDirectory &directories.CliHeader.Directory &writer
    dataDirectory &directories.Reserved &writer

let sectionHeaders info (writer: byref<#IByteWriter>) =
    for i = 0 to info.Sections.Length - 1 do
        let section = &info.Sections.ItemRef i
        let header = section.Header
        writer.Write(SectionName.asSpan header.SectionName)
        writer.WriteLE header.VirtualSize
        writer.WriteLE(uint32 header.VirtualAddress)
        writer.WriteLE header.RawDataSize
        writer.WriteLE(uint32 header.RawDataPointer)
        writer.WriteLE header.PointerToRelocations
        writer.WriteLE header.PointerToLineNumbers
        writer.WriteLE header.NumberOfRelocations
        writer.WriteLE header.NumberOfLineNumbers
        writer.WriteLE(uint32 header.Characteristics)

let sectionData info (writer: byref<FileHeaderWriter<#IByteWriter>>) =
    for section in info.Sections do
        let data = Span.stackalloc<byte> section.Data.ChunkSize
        let mutable remaining = section.Data.Length
        while remaining > 0u do
            data.Clear()

            let length =
                if remaining > uint32 data.Length
                then data.Length
                else int32 remaining
            let destination = data.Slice(0, length)

            section.Data.CopyTo(section.Data.Length - remaining, destination)
            writer.Write destination
            remaining <- remaining - uint32 length
        // Padding to next section.
        writer.AlignTo info.FileAlignment

let internal write file output =
    let mutable output' = FileHeaderWriter output
    let info = getFileInfo file
    output'.Write Magic.msDosStub
    output'.Write Magic.portableExecutableSignature
    coffHeader info &output'
    optionalHeader info &output'
    sectionHeaders info &output'

    // Padding to start of section data
    output'.AlignTo info.FileAlignment

    sectionData info &output'

    output'.Output

let chunkedMemory file = (write file (ChunkedMemoryBuilder(int32 file.OptionalHeader.Alignment.FileAlignment))).ToImmutable()

let block file = (chunkedMemory file).ToImmutableArray()

[<Struct>]
type StreamByteWriter<'Stream when 'Stream :> Stream> (stream: 'Stream) =
    interface IByteWriter with member _.Write data = stream.Write data

let toStream (stream: #Stream) file = write file (StreamByteWriter stream) |> ignore

let toFile (file: FileInfo) pe =
    match file with
    | null -> nullArg "file"
    | _ ->
        use stream = file.OpenWrite()
        toStream stream pe

let toPath path pe =
    use stream = File.OpenWrite path
    toStream stream pe

let stream file = new ChunkedMemoryStream(chunkedMemory file)

[<Struct>]
type ArrayByteWriter = struct
    val mutable private offset: int32
    val private dest: byte[]
    new (destination) = { dest = destination; offset = 0 }
    interface IByteWriter with
        member this.Write data =
            let dest' = Span(this.dest, this.offset, data.Length)
            data.CopyTo dest'
            this.offset <- this.offset + data.Length
end

let toArray file destination = write file (ArrayByteWriter destination) |> ignore
