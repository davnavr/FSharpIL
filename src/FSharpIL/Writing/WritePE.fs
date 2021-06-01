[<RequireQualifiedAccess>]
module FSharpIL.Writing.WritePE

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL
open FSharpIL.PortableExecutable

/// Contains information about the Portable Executable file.
[<NoEquality; NoComparison>]
type PEInfo =
    { FileHeader: CoffHeader<Omitted, Omitted>
      OptionalHeader: OptionalHeader
      CodeSize: uint32
      InitializedDataSize: uint32
      UninitializedDataSize: uint32
      BaseOfCode: Rva
      BaseOfData: Rva
      ImageSize: uint32
      Sections: ImmutableArray<struct(SectionHeader * ReadOnlyMemory<byte>)>
      CliHeaderRva: Rva }

let getFileInfo (file: #IPortableExecutable) =
    let sections, salignment = file.Sections, uint32 file.OptionalHeader.Alignment.SectionAlignment
    let mutable sections', sectioni = Array.zeroCreate sections.Count, 0
    let mutable codeSize, initDataSize, uninitDataSize, imageSize = 0u, 0u, 0u, salignment
    let mutable baseOfCode, baseOfData, rva = Rva.Zero, Rva.Zero, Rva salignment
    for struct(header, data) as section in sections do
        let flags, size = header.Characteristics, uint32 data.Length
        if flags.HasFlag SectionCharacteristics.CntCode then codeSize <- codeSize + size
        if flags.HasFlag SectionCharacteristics.CntInitializedData then initDataSize <- initDataSize + size
        if flags.HasFlag SectionCharacteristics.CntUninitializedData then uninitDataSize <- uninitDataSize + size

        // TODO: Set RVA for BaseOfCode and BaseOfData. Maybe check section flags or section name?

        imageSize <- imageSize + size
        sections'.[sectioni] <- section
        sectioni <- sectioni + 1
        rva <- rva + Round.upTo salignment size
    { FileHeader = file.CoffHeader
      OptionalHeader = file.OptionalHeader
      CodeSize = codeSize
      InitializedDataSize = initDataSize
      UninitializedDataSize = uninitDataSize
      
      ImageSize = imageSize
      Sections = Unsafe.As &sections' }

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

[<Struct>]
type FileHeaderWriter<'Writer when 'Writer :> IByteWriter> = struct
    val private output: 'Writer
    val mutable private pos: uint32
    new (output) = { output = output; pos = 0u }
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

/// Writes the PE optional header (II.25.2.3).
let optionalHeader (file: #IPortableExecutable) (writer: byref<#IByteWriter>) =
    match file.OptionalHeader with
    | PE32(standard, nt) ->
        writer.WriteLE(uint16 ImageKind.PE32)
        let lversion = Span.stackalloc<byte> 2
        lversion.[0] <- standard.LMajor
        lversion.[1] <- standard.LMinor
        writer.Write lversion
        // CodeSize
        ()
    | PE32Plus(standard, nt) ->
        writer.WriteLE(uint16 ImageKind.PE32Plus)
        failwith "TODO: Generation of PE32+ files is not yet supported"

let internal write (file: #IPortableExecutable) (output: #IByteWriter) =
    let mutable output' = FileHeaderWriter output
    let info = getFileInfo file
    output'.Write msDosStub
    output'.Write Magic.portableExecutableSignature
    coffHeader info &output'
    optionalHeader file &output'
    ()

// TODO: When writing to file, can optimize by writing PE file headers directly to underlying stream.
let toStream stream file =
    ()

// TODO: When writing to array or to ChunkedMemory, write PE file headers to there instead.
