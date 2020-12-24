[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open System
open System.Collections.Immutable

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

    /// The signature of the CLI metadata root.
    let CliSignature = [| 0x42uy; 0x53uy; 0x4Auy; 0x42uy; |]

type private SectionInfo =
    { ActualSize: Lazy<uint64>
      DataSizes: ImmutableArray<Lazy<uint64>>
      FileOffset: Lazy<uint64>
      /// The size of the section rounded up to a multiple of FileAlignment
      RoundedSize: Lazy<uint64>
      Section: Section }

[<RequireQualifiedAccess>]
module private LengthOf =
    let PEHeader = DosStub.Length + PESignature.Length |> uint64

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

    let cliMetadata (data: MetadataRoot) =
        let version = MetadataVersion.toArray data.Version
        uint64 CliSignature.Length
        + 2UL // MajorVersion
        + 2UL // MinorVersion
        + 4UL // Reserved
        + 4UL // Length
        + uint64 version.Length
        + 2UL // Flags
        + 2UL // Streams
        // + Other stuff

    /// Calculates the combined length of the CLI header, the strong name hash, the
    /// method bodies, and the CLI metadata.
    let cliData (data: CliHeader) = // TODO: Figure out how to use the cached values from CliInfo.
        CliHeader
        + uint64 data.StrongNameSignature.Length
        // + Method Bodies
        + cliMetadata data.Metadata

    let sectionData =
        function
        | SectionData.CliHeader cli -> cliData cli
        | ClrLoaderStub -> 8UL
        | RawData(Lazy data) -> uint64 data.Length

/// Stores the sizes and file offsets of the various objects and structs that make up a PE file.
[<Sealed>]
type private PEInfo (pe: PEFile) as this =
    member _.File = pe
    // II.25.2.3.1
    member val CodeSize =
        lazy this.SectionsLengthRounded SectionFlags.CntCode
    // II.25.2.3.1
    member val InitializedDataSize =
        lazy this.SectionsLengthRounded SectionFlags.CntInitializedData
    member val UninitializedDataSize =
        lazy this.SectionsLengthRounded SectionFlags.CntUninitializedData
    /// Calculates the size of the headers rounded up to nearest multiple of FileAlignment.
    member val HeaderSizeActual: Lazy<uint64> =
        lazy
            LengthOf.PEHeader
            + LengthOf.CoffHeader
            + LengthOf.StandardFields
            + LengthOf.NTSpecificFields
            + LengthOf.DataDirectories
            + (uint64 pe.SectionTable.Length * LengthOf.SectionHeader)
    member val HeaderSizeRounded: Lazy<uint64> =
        lazy
            Round.upTo
                (uint64 pe.NTSpecificFields.Alignment.FileAlignment)
                this.HeaderSizeActual.Value
    member val Sections: ImmutableArray<_> =
        let sections = ImmutableArray.CreateBuilder pe.SectionTable.Length
        for sectionIndex = 0 to pe.SectionTable.Length - 1 do
            let section = pe.SectionTable.Item sectionIndex
            let data =
                let items = ImmutableArray.CreateBuilder section.Data.Length
                for item in section.Data do
                    lazy LengthOf.sectionData item |> items.Add
                items.ToImmutable()
            let actualSize = lazy Seq.sumBy (|Lazy|) data
            { ActualSize = actualSize
              DataSizes = data // TODO: Figure out if file offset of each data "segment" can be stored as well.
              FileOffset =
                if sectionIndex = 0 then
                    lazy this.HeaderSizeRounded.Value
                else
                    let prevIndex = sectionIndex - 1
                    lazy
                        let prevSection = this.Sections.Item prevIndex
                        prevSection.FileOffset.Value + prevSection.RoundedSize.Value
              RoundedSize =
                lazy
                    Round.upTo
                        (uint64 pe.NTSpecificFields.Alignment.FileAlignment)
                        actualSize.Value
              Section = section }
            |> sections.Add

        sections.ToImmutable()
    member val CliHeaderRva: Lazy<uint64> =
        lazy
            match pe.DataDirectories.CliHeader with
            | Some header ->
                let section = this.Sections.Item header.SectionIndex
                let mutable offset = section.FileOffset.Value
                for i = 0 to header.DataIndex - 1 do
                    let (Lazy prevSize) = section.DataSizes.Item i
                    offset <- offset + prevSize
                offset
            | None -> 0UL
    member _.TotalLength() =
        let sections =
            Seq.sumBy
                (fun { RoundedSize = (Lazy size) } -> size)
                this.Sections
        this.HeaderSizeRounded.Value + sections

    member private _.SectionsLengthRounded (flag: SectionFlags) =
        Seq.where
            (fun { Section = section } -> section.Header.Characteristics.HasFlag flag)
            this.Sections
        |> Seq.sumBy (fun section -> section.RoundedSize.Value) // Apparently the `SizeOfRawData` is used.

[<Sealed>]
type private CliInfo (cli: CliHeader, pe: PEInfo) as this =
    member _.CliHeaderRva: uint64 = pe.CliHeaderRva.Value // NOTE: This value will be incorrect if more than one CliHeader is present.
    member _.StrongNameSignatureRva = this.CliHeaderRva + LengthOf.CliHeader
    member _.StrongNameSignatureSize = uint64 cli.StrongNameSignature.Length
    member val MethodBodiesLength: Lazy<uint64> =
        lazy 0UL
    member val MetaDataRva =
        lazy
            this.CliHeaderRva
            + LengthOf.CliHeader
            + this.StrongNameSignatureSize
            + this.MethodBodiesLength.Value
    member val MetaDataSize =
        lazy
            uint64 CliSignature.Length
            + 2UL // MajorVersion
            + 2UL // MinorVersion
            + 4UL // Reserved
            + 4UL // Length
            + uint64 this.MetaDataVersion.Value.Length
            + 2UL // Flags
            + 2UL // Streams
            // + Other stuff
    member val MetaDataVersion: Lazy<byte[]> = lazy MetadataVersion.toArray cli.Metadata.Version

[<AbstractClass>]
type private ByteWriter<'Result>() =
    abstract member GetResult: unit -> 'Result
    abstract member Write: currentPos: uint64 * byte -> unit
    abstract member Write: currentPos: uint64 * byte[] -> unit
    interface IDisposable with member _.Dispose() = ()

[<Sealed>]
type private Writer<'Result>(writer: ByteWriter<'Result>) =
    let mutable pos = 0UL

    member _.Position = pos

    member _.GetResult() = writer.GetResult()

    member _.Write(bytes: byte[]) =
        if bytes.Length > 0 then
            try writer.Write(pos, bytes)
            with
            | ex ->
                let msg = sprintf "Exception thrown while writing a byte array of length %i at position %i" bytes.Length pos
                InvalidOperationException(msg, ex) |> raise

            // The position is updated afterward, which means that the first byte written is always at position zero.
            pos <- pos + uint64 bytes.Length

    member this.Write(bytes: ImmutableArray<byte>) =
        bytes |> Seq.iter this.WriteU8

    member _.WriteU8(byte: byte) =
        try writer.Write(pos, byte)
        with
        | ex ->
            let msg = sprintf "Exception thrown while writing byte at position %i" pos
            InvalidOperationException(msg, ex) |> raise

        pos <- pos + 1UL

    member inline this.WriteU8 value = this.WriteU8(uint8 value)

    member this.WriteU16(value: uint16) =
        let value' = uint16 value
        this.WriteU8 (value' &&& 0xFFus)
        (value' >>> 8) &&& 0xFFus |> this.WriteU8

    member inline this.WriteU16 value = this.WriteU16(uint16 value)

    member this.WriteU32(value: uint32) =
        let value' = uint32 value
        this.WriteU8 (value' &&& 0xFFu)
        (value' >>> 8) &&& 0xFFu |> this.WriteU8
        (value' >>> 16) &&& 0xFFu |> this.WriteU8
        (value' >>> 24) &&& 0xFFu |> this.WriteU8

    member inline this.WriteU32 value = this.WriteU32(uint32 value)

    member this.WriteEmpty(amt: int) = Array.replicate amt 0uy |> this.Write

    interface IDisposable with member _.Dispose() = (writer :> IDisposable).Dispose()

[<AutoOpen>]
module private Helpers =
    /// Checks if an index is not greater than the maximum allowed index for an array item.
    let (|ValidArrayIndex|) (i: uint64) =
        if i > uint64 Int32.MaxValue 
        then invalidArg "pe" "The PortableExecutable file is too large to fit inside of a byte array."
        else int32 i

    let inline sectionRva (info: PEInfo) =
        function
        | Some(i, _) -> info.Sections.Item(i).FileOffset.Value
        | None -> 0UL

/// Writes the headers and section headers of a PE file.
let private headers (info: PEInfo) (bin: Writer<_>) =
    let pe = info.File
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
    bin.WriteU32 info.CodeSize.Value
    bin.WriteU32 info.InitializedDataSize.Value
    bin.WriteU32 info.UninitializedDataSize.Value
    // NOTE: The EntryPointRva always has a value regardless of whether or not it is a .dll or .exe, and points to somewhere special (see the end of II.25.2.3.1)
    bin.WriteEmpty 4 // EntryPointRva // TODO: Figure out what this value should be.
    sectionRva info pe.Sections.TextSection |> bin.WriteU32 // BaseOfCode, matches the RVA of the .text section
    sectionRva info pe.Sections.TextSection |> bin.WriteU32 // BaseOfData, matches the RVA of the .rsrc section

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
    bin.WriteU32 info.HeaderSizeRounded.Value
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

    if pe.CliHeader.IsSome then
        bin.WriteU32 info.CliHeaderRva.Value // CliHeader
        bin.WriteU32 LengthOf.CliHeader
    else bin.WriteEmpty 8

    bin.WriteEmpty 8 // Reserved

    for i = 0 to pe.SectionTable.Length - 1 do
        let section = info.Sections.Item(i)
        let header = section.Section.Header
        SectionName.toArray header.SectionName |> bin.Write
        bin.WriteU32 section.ActualSize.Value
        // TODO: Figure out how to calculate these fields from the data
        bin.WriteU32 section.FileOffset.Value // VirtualAddress
        bin.WriteU32 section.RoundedSize.Value
        bin.WriteU32 section.FileOffset.Value // PointerToRawData
        bin.WriteU32 header.PointerToRelocations
        bin.WriteEmpty 4 // PointerToLineNumbers
        bin.WriteU16 header.NumberOfRelocations
        bin.WriteEmpty 2 // NumberOfLineNumbers
        bin.WriteU32 header.Characteristics

    // Padding separating headers from the sections
    info.HeaderSizeRounded.Value - info.HeaderSizeActual.Value |> int |> bin.WriteEmpty

let private cli (pe: PEInfo) (header: CliHeader) (bin: Writer<_>) =
    let info = CliInfo(header, pe)
    bin.WriteU32 LengthOf.CliHeader
    bin.WriteU16 header.MajorRuntimeVersion
    bin.WriteU16 header.MinorRuntimeVersion
    bin.WriteU32 info.MetaDataRva.Value
    bin.WriteU32 info.MetaDataSize.Value
    bin.WriteU32 header.Flags
    bin.WriteEmpty 4 // EntryPointToken

    bin.WriteEmpty 4 // RVA of Resources
    bin.WriteEmpty 4 // Size of Resources

    if header.StrongNameSignature.IsEmpty
    then bin.WriteEmpty 8
    else
        bin.WriteU32 info.StrongNameSignatureRva
        bin.WriteU32 info.StrongNameSignatureSize

    bin.WriteEmpty 8 // CodeManagerTable
    bin.WriteEmpty 8 // VTableFixups // TODO: See if this needs to be assigned a value
    bin.WriteEmpty 8 // ExportAddressTableJumps
    bin.WriteEmpty 8 // ManagedNativeHeader

    bin.Write header.StrongNameSignature

    // TODO: Write method bodies

    let root = header.Metadata
    bin.Write CliSignature
    bin.WriteU16 root.MajorVersion
    bin.WriteU16 root.MinorVersion
    bin.WriteEmpty 4 // Reserved
    bin.WriteU32 root.Version.Length
    bin.Write info.MetaDataVersion.Value
    bin.WriteEmpty 2 // Flags
    bin.WriteU16 root.Streams.Count

    // TODO: Write stream headers.

let private write pe (writer: PEInfo -> ByteWriter<_>) =
    let info = PEInfo pe
    let bin = new Writer<_> (writer info)

    headers info bin

    for section in info.Sections do
        let pos = bin.Position
        let fileOffset = section.FileOffset.Value

        if pos <> fileOffset then
            sprintf
                "The file offset of the section (0x%X) did not match the current position of the writer (0x%X)"
                fileOffset
                pos
            |> invalidOp

        for data in section.Section.Data do
            match data with
            | RawData(Lazy bytes)-> bin.Write bytes
            | CliHeader header -> cli info header bin
            | ClrLoaderStub -> bin.WriteEmpty 8 // TODO: Write the loader stub

        section.RoundedSize.Value - section.ActualSize.Value |> int |> bin.WriteEmpty

    bin.GetResult()

let toArray pe =
    write
        pe
        (fun info ->
            let (ValidArrayIndex totalLength) = info.TotalLength()
            let bytes = Array.zeroCreate<byte> totalLength
            { new ByteWriter<_>() with
                override _.GetResult() = bytes
                override _.Write(ValidArrayIndex pos, value) =
                    Array.set bytes pos value
                override _.Write(ValidArrayIndex pos, source: byte[]) =
                    Array.Copy(source, 0, bytes, pos, source.Length) })
