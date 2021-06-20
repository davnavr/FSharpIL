﻿[<RequireQualifiedAccess>]
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

        imageSize <- imageSize + size
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

/// Writes the PE optional header (II.25.2.3).
let optionalHeader info (writer: byref<#IByteWriter>) =
    match info.OptionalHeader with
    | PE32(standard, nt) ->
        standardFieldsCommon ImageKind.PE32Plus standard info &writer
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
    noImpl "TODO: Write data directories"

let internal write file output =
    let mutable output' = FileHeaderWriter output
    let info = getFileInfo file
    output'.Write Magic.msDosStub
    output'.Write Magic.portableExecutableSignature
    coffHeader info &output'
    optionalHeader info &output'

    // Padding to start of section data
    let mutable padding = (Round.upTo info.FileAlignment output'.Position) - output'.Position
    while padding > 0u do
        let padding' = Span.stackalloc<byte>(int32(min padding info.FileAlignment))
        padding'.Clear()
        output'.Write padding'
        padding <- padding - uint32 padding'.Length

    // Section data
    for section in info.Sections do
        let data = Span.stackalloc<byte> section.Data.ChunkSize
        section.Data.CopyTo(0u, data)
        output'.Write data
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
