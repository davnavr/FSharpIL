[<RequireQualifiedAccess>]
module FSharpIL.Writing.WritePE

open System
open System.Collections.Immutable
open System.IO

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.PortableExecutable

/// The MS-DOS header, which contains a pointer to the PE signature (II.25.2.1).
let msDosStub =
    [|
        0x4duy; 0x5auy; 0x90uy; 0x00uy; 0x03uy; 0x00uy; 0x00uy; 0x00uy;
        0x04uy; 0x00uy; 0x00uy; 0x00uy; 0xFFuy; 0xFFuy; 0x00uy; 0x00uy;
        0xb8uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
        0x40uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
        0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
        0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
        0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
        0x00uy; 0x00uy; 0x00uy; 0x00uy;
        0x80uy; 0x00uy; 0x00uy; 0x00uy; // lfanew
        0x0euy; 0x1fuy; 0xbauy; 0x0euy; 0x00uy; 0xb4uy; 0x09uy; 0xcduy;
        0x21uy; 0xb8uy; 0x01uy; 0x4cuy; 0xcduy; 0x21uy; 0x54uy; 0x68uy;
        0x69uy; 0x73uy; 0x20uy; 0x70uy; 0x72uy; 0x6fuy; 0x67uy; 0x72uy;
        0x61uy; 0x6duy; 0x20uy; 0x63uy; 0x61uy; 0x6euy; 0x6euy; 0x6fuy;
        0x74uy; 0x20uy; 0x62uy; 0x65uy; 0x20uy; 0x72uy; 0x75uy; 0x6euy;
        0x20uy; 0x69uy; 0x6euy; 0x20uy; 0x44uy; 0x4fuy; 0x53uy; 0x20uy;
        0x6duy; 0x6fuy; 0x64uy; 0x65uy; 0x2euy; 0x0duy; 0x0duy; 0x0auy;
        0x24uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
    |]

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

    let headersSize =
        let optionalHeaderSize =
            match optionalHeader with
            | PE32 _ -> Magic.optionalHeaderSize
            | PE32Plus _ -> failwith "TODO: Get size of PE32+ header"
        uint32 msDosStub.Length
        + uint32 Magic.portableExecutableSignature.Length
        + uint32 optionalHeaderSize+ (Magic.sectionHeaderSize * uint32 sections.Length)
        |> Round.upTo falignment
    let mutable codeSize, initDataSize, uninitDataSize, imageSize = 0u, 0u, 0u, Round.upTo salignment headersSize
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
      HeadersSize = headersSize
      Sections = file.Sections
      CliHeaderRva = dataDirectories.CliHeader.CliHeader.Rva }

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
    writer.WriteLE Magic.optionalHeaderSize
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

let ntSpecificFieldsCommon (fields: NTSpecificFields<_, Alignment, _, _, _>) info (writer: byref<#IByteWriter>) =
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

let internal write file output =
    let mutable output' = FileHeaderWriter output
    let info = getFileInfo file
    output'.Write msDosStub
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
