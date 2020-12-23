[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open System
open System.IO

open FSharpIL.PortableExecutable
open FSharpIL.Utilities

type Builder internal() =
    member _.Delay(f): PEFile = f()
    member _.Zero(): PEFile = PEFile.Default

type private Writer(stream: Stream) =
    let writer = new BinaryWriter(stream)
    let mutable pos = 0UL

    member _.Position = pos

    member _.Write(bytes: byte[]) =
        pos <- pos + uint64 bytes.Length
        writer.Write bytes

    member _.WriteU8(byte: byte) =
        pos <- pos + 1UL
        writer.Write byte

    member this.WriteU16(value: uint16) =
        let value' = uint16 value
        this.Write [| byte (value' &&& 0xFFus); (value' >>> 8) &&& 0xFFus |> byte |]

    member inline this.WriteU16 value = this.WriteU16(uint16 value)

    member this.WriteU32(value: uint32) =
        let value' = uint32 value
        [|
            byte (value' &&& 0xFFu)
            (value' >>> 8) &&& 0xFFu |> byte
            (value' >>> 16) &&& 0xFFu |> byte
            (value' >>> 24) &&& 0xFFu |> byte
        |]
        |> this.Write

    member inline this.WriteU32 value = this.WriteU32(uint32 value)

    member this.WriteEmpty(amt: int) = Array.replicate amt 0uy |> this.Write

    member inline this.WriteEmpty(num: ^T) =
        let mutable i = LanguagePrimitives.GenericZero
        while i < num do
            i <- i + LanguagePrimitives.GenericOne
            this.WriteU8 0uy

    member _.SeekStart() =
        let max = uint64 Int32.MaxValue
        while pos > 0UL do
            let offset =
                if pos >= max
                then uint64 Int32.MaxValue
                else pos
            pos <- pos - offset
            writer.Seek(-(int offset), SeekOrigin.Current) |> ignore
        assert (pos = 0UL)

    interface System.IDisposable with member _.Dispose() = writer.Dispose()

let private cli (header: Metadata.CliHeader) (bin: Writer) =
    /// File offset to CLI header
    let start = bin.Position
    bin.WriteU32 Length.CliHeader
    bin.WriteU16 header.MajorRuntimeVersion
    bin.WriteU16 header.MinorRuntimeVersion
    bin.WriteEmpty 4 // RVA of MetaData
    bin.WriteEmpty 4 // Size of MetaData
    bin.WriteU32 header.Flags
    bin.WriteEmpty 4 // EntryPointToken
    bin.WriteEmpty 4 // RVA of Resources
    bin.WriteEmpty 4 // Size of Resources
    bin.WriteEmpty 4 // RVA of StrongNameSignature
    bin.WriteEmpty 4 // Size of StrongNameSignature
    bin.WriteEmpty 8 // CodeManagerTable
    bin.WriteEmpty 8 // VTableFixups // TODO: See if this needs to be assigned a value
    bin.WriteEmpty 8 // ExportAddressTableJumps
    bin.WriteEmpty 8 // ManagedNativeHeader

    // TODO: Write strong name hash (StrongNameSignature) if needed
    // TODO: Write method bodies if needed
    // TODO: Write CLR metadata

    // TODO: Figure out how to write the CLR metadata first, and then inserting the header before it to allow the metadata size to be measured.

    start

let private write (file: PEFile) (bin: Writer) =
    let coff = file.FileHeader
    let standard = file.StandardFields
    let nt = file.NTSpecificFields

    let sectionHeadersSize = 40u * uint32 file.SectionTable.Length
    let headerSizeActual, headerSizeRounded =
        let actual =
            uint dosStub.Length
            + uint peSignature.Length
            + 20u // Coff
            + 28u // Standard (PE32)
            + 68u // NT
            + 128u // Data Directories, of which there are 16
            + sectionHeadersSize
        actual, Round.upTo nt.Alignment.FileAlignment actual

    bin.WriteEmpty headerSizeRounded // Writes the bytes where the headers will eventually go.

    let mutable cliHeader = 0UL

    let sections =
        Array.init
            file.SectionTable.Length
            (fun i ->
                let falignment = uint64 nt.Alignment.FileAlignment
                let section = file.SectionTable.Item i
                let pos = bin.Position
                for data in section.Data do
                    match data with
                    | RawData bytes -> bytes() |> bin.Write
                    | CliHeader header ->
                        cliHeader <- cli header bin
                    | ClrLoaderStub -> bin.WriteEmpty 8 // TODO: Write the loader stub
                let size = bin.Position - pos
                let rsize = Round.upTo falignment size
                rsize - size |> bin.WriteEmpty // Padding
                assert (bin.Position % uint64 nt.Alignment.FileAlignment = 0UL)
                struct
                    {| ActualSize = size
                       /// The location of the section on disk. Guaranteed to be a multiple of FileAlignment
                       FileOffset = pos
                       RoundedSize = rsize |})

    bin.SeekStart()

    bin.WriteEmpty (headerSizeActual - sectionHeadersSize) // TODO: Instead of writing empty bytes, try using seeking here instead.

    for i = 0 to file.SectionTable.Length - 1 do
        let header = file.SectionTable.Item(i).Header
        let info = Array.item i sections
        SectionName.toArray header.SectionName |> bin.Write
        bin.WriteU32 info.ActualSize
        // TODO: Figure out how to calculate these fields from the data
        bin.WriteU32 info.FileOffset // VirtualAddress
        bin.WriteU32 info.RoundedSize
        bin.WriteU32 info.FileOffset // PointerToRawData
        bin.WriteU32 header.PointerToRelocations
        bin.WriteEmpty 4 // PointerToLineNumbers
        bin.WriteU16 header.NumberOfRelocations
        bin.WriteEmpty 2 // NumberOfLineNumbers
        bin.WriteU32 header.Characteristics

    bin.SeekStart()
    bin.Write dosStub
    bin.Write peSignature

    bin.WriteU16 coff.Machine
    bin.WriteU16 file.SectionTable.Length
    bin.WriteU32 coff.TimeDateStamp
    bin.WriteU32 coff.SymbolTablePointer
    bin.WriteU32 coff.SymbolCount
    bin.WriteU16 0xE0us // OptionalHeaderSize
    bin.WriteU16 coff.Characteristics

    bin.WriteU16 0x10Bus // Magic, 0x10B means PE32
    bin.WriteU8 standard.LMajor
    bin.WriteU8 standard.LMinor

    match file.SectionInfo.TextSection with
    | Some(i, _) -> (Array.item i sections).ActualSize
    | None -> 0UL
    |> bin.WriteU32 // CodeSize

    // TODO: Figure out how to calculate the values for the rest of the standard fields
    bin.WriteU32 1u // InitializedDataSize
    bin.WriteU32 1u // UninitizalizedDataSize
    bin.WriteEmpty 12 // TEMPORARY
    // EntryPointRva
    // BaseOfCode // NOTE: This matches the RVA of the .text section
    // BaseOfData // NOTE: This matches the RVA of the .rsrc section

    bin.WriteU32 nt.ImageBase
    bin.WriteU32 nt.Alignment.SectionAlignment
    bin.WriteU32 nt.Alignment.FileAlignment
    bin.WriteU16 nt.OSMajor
    bin.WriteU16 nt.OSMinor
    bin.WriteU16 nt.UserMajor
    bin.WriteU16 nt.UserMinor
    bin.WriteU16 nt.SubSysMajor
    bin.WriteU16 nt.SubSysMinor
    bin.WriteU32 nt.Win32VersionValue
    bin.WriteEmpty 4 // ImageSize // TODO: Figure out how to calculate the ImageSize
    bin.WriteU32 headerSizeRounded
    bin.WriteU32 nt.FileChecksum
    bin.WriteU16 nt.Subsystem
    bin.WriteU16 nt.DllFlags
    bin.WriteU32 nt.StackReserveSize
    bin.WriteU32 nt.StackCommitSize
    bin.WriteU32 nt.HeapReserveSize
    bin.WriteU32 nt.HeapCommitSize
    bin.WriteU32 nt.LoaderFlags
    bin.WriteU32 0x10u // NumberOfDataDirectories

    bin.WriteEmpty 8 // ExportTable
    bin.WriteEmpty 8 // TEMPORARY // ImportTable
    bin.WriteEmpty 8 // ResourceTable
    bin.WriteEmpty 8 // ExceptionTable
    bin.WriteEmpty 8 // CertificateTable
    bin.WriteEmpty 8 // TEMPORARY // BaseRelocationTable
    bin.WriteEmpty 8 // DebugTable
    bin.WriteEmpty 8 // CopyrightTable
    bin.WriteEmpty 8 // GlobalPointerTable
    bin.WriteEmpty 8 // TLSTable
    bin.WriteEmpty 8 // LoadConfigTable
    bin.WriteEmpty 8 // BoundImportTable
    bin.WriteEmpty 8 // TEMPORARY // ImportAddressTable
    bin.WriteEmpty 8 // DelayImportDescriptor
    bin.WriteU32 cliHeader // CliHeader // NOTE: RVA points to the CliHeader
    bin.WriteU32 Length.CliHeader // TODO: What happens if a header is not specified?
    bin.WriteEmpty 8 // Reserved
    // Section headers immediately follow the optional header

let private signature =
    [|
        0x4duy; 0x5auy; 0x90uy; 0x00uy; 0x03uy; 0x00uy; 0x00uy; 0x00uy;
        0x04uy; 0x00uy; 0x00uy; 0x00uy; 0xFFuy; 0xFFuy; 0x00uy; 0x00uy;
        0xb8uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
        0x40uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
        0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
        0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
        0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
        0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy; // lfanew
        0x0euy; 0x1fuy; 0xbauy; 0x0euy; 0x00uy; 0xb4uy; 0x09uy; 0xcduy;
        0x21uy; 0xb8uy; 0x01uy; 0x4cuy; 0xcduy; 0x21uy; 0x54uy; 0x68uy;
        0x69uy; 0x73uy; 0x20uy; 0x70uy; 0x72uy; 0x6fuy; 0x67uy; 0x72uy;
        0x61uy; 0x6duy; 0x20uy; 0x63uy; 0x61uy; 0x6euy; 0x6euy; 0x6fuy;
        0x74uy; 0x20uy; 0x62uy; 0x65uy; 0x20uy; 0x72uy; 0x75uy; 0x6euy;
        0x20uy; 0x69uy; 0x6euy; 0x20uy; 0x44uy; 0x4fuy; 0x53uy; 0x20uy;
        0x6duy; 0x6fuy; 0x64uy; 0x65uy; 0x2euy; 0x0duy; 0x0duy; 0x0auy;
        0x24uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy;
        0x50uy; 0x45uy; 0x00uy; 0x00uy;
    |]

[<RequireQualifiedAccess>]
module private Header =
    let coff (header: CoffHeader) =
        [| |]

// NOTE: Since stream seeking is used, find a way to ensure that the written data is written at the end of the stream.
// NOTE: Implementors of the stream class could break something, perhaps use a different stream under the hood?
let toStream stream pe =
    use writer = new Writer(stream)
    write pe writer
