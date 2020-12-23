[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open System
open System.IO

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

[<AutoOpen>]
module private Magic =
    let DosStub =
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

    let PESignature = "PE\000\000"B

    /// Magic number used in the optional header that identifies the file as a PE32 executable.
    [<Literal>]
    let PE32 = 0x10Bus

[<RequireQualifiedAccess>]
module private LengthOf =
    let peHeader = DosStub.Length + PESignature.Length |> uint64

    [<Literal>]
    let CoffHeader = 20UL

    /// The value of the SizeOfOptionalHeader field in the COFF file header.
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

    /// The length of the CLI header.
    [<Literal>]
    let CliHeader = 0x48UL

    /// Calculates the combined length of the CLI header, the strong name hash, the
    /// method bodies, and the CLI metadata.
    let cliData (data: CliHeader) =
        CliHeader
        // + Strong Name Hash
        // + Method Bodies
        // + Metadata

    let section (section: Section) =
        let mutable len = 0UL
        for data in section.Data do
            let len' =
                match data with
                | SectionData.CliHeader cli -> cliData cli
                | ClrLoaderStub -> 8UL
                | RawData data -> data() |> Array.length |> uint64
            len <- len + len'
        len

/// Stores the lengths of various the objects and structs that make up a PE file.
type private PELength(pe: PEFile) as this =
    member val SizeOfCode =
        match pe.SectionInfo.TextSection with
        | Some(i, _) -> Array.item i this.SectionLengths
        | None -> lazy 0UL
    member val SectionHeadersLength: Lazy<_> =
        lazy (uint64 pe.SectionTable.Length * LengthOf.SectionHeader)
    /// Calculates the size of the headers rounded up to nearest multiple of FileAlignment.
    member val HeaderSizeRounded: Lazy<_> =
        lazy (
            let actual =
                LengthOf.peHeader
                + LengthOf.CoffHeader
                + LengthOf.StandardFields
                + LengthOf.NTSpecificFields
                + LengthOf.DataDirectories
                + this.SectionHeadersLength.Value
            Round.upTo
                (uint64 pe.NTSpecificFields.Alignment.FileAlignment)
                actual
        )
    member val SectionLengths: Lazy<_>[] =
        Seq.map
            (fun section -> lazy (LengthOf.section section))
            pe.SectionTable
        |> Seq.toArray
    member val TotalLength =
        lazy (
            LengthOf.peHeader
            + LengthOf.CoffHeader
            + LengthOf.StandardFields
            + LengthOf.NTSpecificFields
            + LengthOf.DataDirectories
            + this.SectionHeadersLength.Value
            // + the padding before the start of the section data.
            // TODO: Add other stuff
        )

[<AbstractClass>]
type private ByteWriter<'Result>() =
    abstract member GetResult: unit -> 'Result
    abstract member Write: currentPos: uint64 * byte -> unit
    abstract member WriteBytes: currentPos: uint64 * byte[] -> unit
    interface IDisposable with member this.Dispose() = ()

type private Writer<'Result>(writer: ByteWriter<'Result>) =
    let stream = invalidOp "TODO: Replace stream with ByteWriter"
    let writer1 = new BinaryWriter(stream)
    let mutable pos = 0UL

    member _.Position = pos

    member _.GetResult() = writer.GetResult()

    member _.Write(bytes: byte[]) =
        writer1.Write bytes
        // The position is updated afterward, which means that the first byte written is always at position zero.
        pos <- pos + uint64 bytes.Length

    member _.WriteU8(byte: byte) =
        writer1.Write byte
        pos <- pos + 1UL

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

    member inline this.WriteEmpty(amt: ^T) =
        let mutable i = LanguagePrimitives.GenericZero
        while i < amt do
            i <- i + LanguagePrimitives.GenericOne
            this.WriteU8 0uy

    interface IDisposable with member _.Dispose() = writer1.Dispose()

let private write pe (writer: PELength -> ByteWriter<_>) =
    let length = PELength pe
    let bin = new Writer<_> (writer length)

    bin.Write DosStub
    bin.Write PESignature

    let coff = pe.FileHeader
    bin.WriteU16 coff.Machine
    bin.WriteU16 pe.SectionTable.Length
    bin.WriteU32 coff.TimeDateStamp
    bin.WriteU32 coff.SymbolTablePointer
    bin.WriteU32 coff.SymbolCount
    bin.WriteU16 LengthOf.OptionalHeader
    bin.WriteU16 coff.Characteristics

    bin.WriteU16 PE32
    let standard = pe.StandardFields
    bin.WriteU8 standard.LMajor
    bin.WriteU8 standard.LMinor
    bin.WriteU32 length.SizeOfCode.Value
    // TODO: Figure out how to calculate the values for the rest of the standard fields
    bin.WriteU32 1u // InitializedDataSize
    bin.WriteU32 1u // UninitizalizedDataSize
    bin.WriteEmpty 12 // TEMPORARY
    // EntryPointRva
    // BaseOfCode // NOTE: This matches the RVA of the .text section
    // BaseOfData // NOTE: This matches the RVA of the .rsrc section

    let nt = pe.NTSpecificFields
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
    bin.WriteU32 length.HeaderSizeRounded.Value
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
    bin.WriteU32 LengthOf.CliHeader // TODO: What happens if a header is not specified?
    bin.WriteEmpty 8 // Reserved

    // TODO: Write section headers

    bin.GetResult()

let toArray pe =
    let (|ValidIndex|) (i: uint64) =
        if i > uint64 Int32.MaxValue 
        then invalidArg "pe" "The PortableExecutable file is too large to fit inside of a byte array."
        else int32 i
    write
        pe
        (fun length ->
            let (Lazy (ValidIndex totalLength)) = length.TotalLength
            let bytes = Array.zeroCreate<byte> totalLength
            { new ByteWriter<_>() with
                override _.GetResult() = bytes
                override _.Write(ValidIndex pos, value) =
                    Array.set bytes pos value
                override _.WriteBytes(ValidIndex pos, source) =
                    Array.Copy(source, 0, bytes, pos, source.Length) })
