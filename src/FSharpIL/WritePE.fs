[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open System
open System.IO

open FSharpIL.PortableExecutable
open FSharpIL.Utilities

type Builder internal() =
    member _.Combine(headers: PEHeaders, pe: PEFile) =
        { pe with Headers = headers }
    member _.Delay(f): PEFile = f()
    member _.Yield(headers: PEHeaders) = headers
    member _.Zero(): PEFile = PEFile.Default

[<AutoOpen>]
module private Helpers =
    let inline (|ValidStream|) (stream: Stream) =
        if stream.CanSeek && stream.CanWrite
        then stream
        else
            invalidArg "stream" "The stream should support writing and seeking."

    let dosstub =
        let lfanew = [| 0x80; 0x00; 0x00; 0x00 |]
        [|
            0x4d; 0x5a; 0x90; 0x00; 0x03; 0x00; 0x00; 0x00
            0x04; 0x00; 0x00; 0x00; 0xFF; 0xFF; 0x00; 0x00;
            0xb8; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
            0x40; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
            0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
            0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
            0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
            0x00; 0x00; 0x00; 0x00; yield! lfanew
            0x0e; 0x1f; 0xba; 0x0e; 0x00; 0xb4; 0x09; 0xcd;
            0x21; 0xb8; 0x01; 0x4c; 0xcd; 0x21; 0x54; 0x68;
            0x69; 0x73; 0x20; 0x70; 0x72; 0x6f; 0x67; 0x72;
            0x61; 0x6d; 0x20; 0x63; 0x61; 0x6e; 0x6e; 0x6f;
            0x74; 0x20; 0x62; 0x65; 0x20; 0x72; 0x75; 0x6e;
            0x20; 0x69; 0x6e; 0x20; 0x44; 0x4f; 0x53; 0x20;
            0x6d; 0x6f; 0x64; 0x65; 0x2e; 0x0d; 0x0d; 0x0a;
            0x24; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
        |]
        |> Array.map byte

    let pesig = "PE\000\000"B

    [<Literal>]
    let CliHeaderSize = 0x48u

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
        assert(pos = 0UL)

    interface System.IDisposable with
        member _.Dispose() = writer.Dispose()

let private cli (header: Metadata.CliHeader) (bin: Writer) =
    let pos = bin.Position
    bin.WriteU32 CliHeaderSize
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

    pos

let private write (file: PEFile) (bin: Writer) =
    let coff = file.Headers.FileHeader
    let standard = file.Headers.StandardFields
    let nt = file.Headers.NTSpecificFields

    let sectionHeadersSize = 40u * uint32 file.SectionTable.Length
    let headerSizeActual, headerSizeRounded =
        let actual =
            uint dosstub.Length
            + uint pesig.Length
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
    bin.Write dosstub
    bin.Write pesig

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
    bin.WriteU32 CliHeaderSize // TODO: What happens if a header is not specified?
    bin.WriteEmpty 8 // Reserved
    // Section headers immediately follow the optional header

// NOTE: Since stream seeking is used, find a way to ensure that the written data is written at the end of the stream.
// NOTE: Implementors of the stream class could break something, perhaps use a different stream under the hood?
let toStream (ValidStream stream) pe =
    use writer = new Writer(stream)
    write pe writer
