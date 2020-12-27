[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open System
open System.Collections.Immutable

open FSharpIL.Magic
open FSharpIL.Metadata
open FSharpIL.PortableExecutable

[<AbstractClass>]
type ByteWriter<'Result>() =
    abstract member GetResult: unit -> 'Result
    abstract member Write: currentPos: uint64 * byte -> unit
    abstract member Write: currentPos: uint64 * byte[] -> unit
    interface IDisposable with member _.Dispose() = ()

[<Sealed>]
type Writer<'Result>(writer: ByteWriter<'Result>) =
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

    member _.Write(byte: byte) =
        try writer.Write(pos, byte)
        with
        | ex ->
            let msg = sprintf "Exception thrown while writing byte at position %i" pos
            InvalidOperationException(msg, ex) |> raise

        pos <- pos + 1UL

    interface IDisposable with member _.Dispose() = (writer :> IDisposable).Dispose()

type WriteExpr<'Result> = Writer<'Result> -> unit

// TODO: Maybe figure out how to keep track of and calculate lengths here.
type Writer() =
    member inline _.Combine(one: WriteExpr<_>, two: WriteExpr<_>) = fun writer -> one writer; two writer
    member inline _.Delay(f: unit -> WriteExpr<_>) = fun writer -> f () writer
    member inline _.YieldFrom(f: WriteExpr<_>) = f
    member inline _.For(items: seq<'T>, body: 'T -> WriteExpr<_>) =
        fun writer -> for item in items do body item writer
    member inline _.Yield(value: byte) = fun (writer: Writer<_>) -> writer.Write value
    member inline _.Yield(bytes: byte[]) = fun (writer: Writer<_>) -> writer.Write bytes
    member inline _.Yield(bytes: seq<byte>) = fun (writer: Writer<_>) -> bytes |> Seq.iter writer.Write
    member inline _.Yield(value: uint16) =
        fun (writer: Writer<_>) ->
            byte (value &&& 0xFFus) |> writer.Write
            (value >>> 8) &&& 0xFFus |> byte |> writer.Write
    member inline _.Yield(value: uint32) =
        fun (writer: Writer<_>) ->
            byte (value &&& 0xFFu) |> writer.Write
            (value >>> 8) &&& 0xFFu |> byte |> writer.Write
            (value >>> 16) &&& 0xFFu |> byte |> writer.Write
            (value >>> 24) &&& 0xFFu |> byte |> writer.Write
    member inline _.Yield(value: uint64) =
        fun (writer: Writer<_>) ->
            byte (value &&& 0xFFUL) |> writer.Write
            (value >>> 8) &&& 0xFFUL |> byte |> writer.Write
            (value >>> 16) &&& 0xFFUL |> byte |> writer.Write
            (value >>> 24) &&& 0xFFUL |> byte |> writer.Write
            (value >>> 32) &&& 0xFFUL |> byte |> writer.Write
            (value >>> 40) &&& 0xFFUL |> byte |> writer.Write
            (value >>> 48) &&& 0xFFUL |> byte |> writer.Write
            (value >>> 56) &&& 0xFFUL |> byte |> writer.Write
    member inline _.Zero() = ignore<Writer<_>>

let writer = Writer()

[<RequireQualifiedAccess>]
module private SizeOf =
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

type private SectionInfo =
    { ActualSize: Lazy<uint64>
      DataSizes: ImmutableArray<Lazy<uint64>>
      FileOffset: Lazy<uint64>
      /// The size of the section rounded up to a multiple of FileAlignment
      RoundedSize: Lazy<uint64>
      Section: Section }

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
            SizeOf.PEHeader
            + SizeOf.CoffHeader
            + SizeOf.StandardFields
            + SizeOf.NTSpecificFields
            + SizeOf.DataDirectories
            + (uint64 pe.SectionTable.Length * SizeOf.SectionHeader)
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
                    lazy
                        match item with
                        | CliHeader cli ->
                            if Some cli = pe.CliHeader
                            then (Option.get this.CliHeader.Value).TotalLength()
                            else invalidOp "Multiple CLI headers are not supported, and may never be supported in the future"
                        | ClrLoaderStub -> 8UL
                        | RawData(Lazy data) -> uint64 data.Length
                    |> items.Add
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
    member val CliHeader: Lazy<CliInfo option> =
        lazy
            Option.map
                (fun cli -> CliInfo(cli, this))
                pe.CliHeader
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

and [<Sealed>] private CliInfo (cli: CliHeader, pe: PEInfo) as this =
    member _.CliHeaderRva: uint64 = pe.CliHeaderRva.Value // NOTE: This value will be incorrect if more than one CliHeader is present.
    member _.StrongNameSignatureRva = this.CliHeaderRva + SizeOf.CliHeader
    member _.StrongNameSignatureSize = uint64 cli.StrongNameSignature.Length
    member val MethodBodiesLength: Lazy<uint64> =
        lazy 0UL
    member val MetaDataRva: Lazy<uint64> =
        lazy
            this.CliHeaderRva
            + SizeOf.CliHeader
            + this.StrongNameSignatureSize
            + this.MethodBodiesLength.Value
    member val MetaDataRootSize: Lazy<uint64> =
        lazy
            uint64 CliSignature.Length
            + 2UL // MajorVersion
            + 2UL // MinorVersion
            + 4UL // Reserved
            + 4UL // Length
            + uint64 this.MetaDataVersion.Value.Length
            + 2UL // Flags
            + 2UL // Streams
            + this.StreamHeadersSize.Value
    member val MetaDataVersion: Lazy<byte[]> = lazy MetadataVersion.toArray cli.Metadata.Version
    member val StreamHeadersSize: Lazy<uint64> =
        lazy
            // #~
            8UL + uint64 MetadataTableStreamName.Length
    /// Calculates the size of all of the streams in the CLI metadata.
    member val MetaDataStreamsSize: Lazy<uint64> =
        lazy this.MetaDataTableSize.Value
    /// Location of the `#~` stream.
    member inline _.MetaDataTableOffset = this.MetaDataRootSize
    member val MetaDataTableSize: Lazy<uint64> = // NOTE: Must be rounded to a multiple of 4.
        lazy
            24UL
            // + Rows
            // + Tables
            // TODO: + Size of Module table?
    member _.TotalLength() =
        SizeOf.CliHeader
        + this.StrongNameSignatureSize
        // + Method Bodies
        + this.MetaDataRootSize.Value
        + this.MetaDataStreamsSize.Value

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

    let empty amt (writer: Writer<_>) =
        let mutable i = 0UL
        while i < amt do
            writer.Write 0uy
            i <- i + 1UL

/// Writes the headers and section headers of a PE file.
let private headers (info: PEInfo) =
    writer {
        let pe = info.File
        DosStub
        PESignature

        let coff = pe.FileHeader
        uint16 coff.Machine
        uint16 pe.SectionTable.Length
        coff.TimeDateStamp
        coff.SymbolTablePointer
        coff.SymbolCount
        SizeOf.OptionalHeader
        uint16 coff.Characteristics

        let standard = pe.StandardFields
        PE32
        standard.LMajor
        standard.LMinor
        uint32 info.CodeSize.Value
        uint32 info.InitializedDataSize.Value
        uint32 info.UninitializedDataSize.Value
        // NOTE: The EntryPointRva always has a value regardless of whether or not it is a .dll or .exe, and points to somewhere special (see the end of II.25.2.3.1)
        0u // EntryPointRva // TODO: Figure out what this value should be.
        sectionRva info pe.Sections.TextSection |> uint32 // BaseOfCode, matches the RVA of the .text section
        sectionRva info pe.Sections.TextSection |> uint32 // BaseOfData, matches the RVA of the .rsrc section

        let nt = pe.NTSpecificFields
        uint32 nt.ImageBase
        nt.Alignment.SectionAlignment
        nt.Alignment.FileAlignment
        nt.OSMajor
        nt.OSMinor
        nt.UserMajor
        nt.UserMinor
        nt.SubSysMajor
        nt.SubSysMinor
        nt.Win32VersionValue
        0u // ImageSize // TODO: Figure out how to calculate the ImageSize
        uint32 info.HeaderSizeRounded.Value
        nt.FileChecksum
        uint16 nt.Subsystem
        uint16 nt.DllFlags
        nt.StackReserveSize
        nt.StackCommitSize
        nt.HeapReserveSize
        nt.HeapCommitSize
        nt.LoaderFlags
        0x10u // NumberOfDataDirectories

        0UL // ExportTable
        0UL // TEMPORARY // ImportTable
        0UL // ResourceTable
        0UL // ExceptionTable
        0UL // CertificateTable
        0UL // TEMPORARY // BaseRelocationTable
        0UL // DebugTable
        0UL // CopyrightTable
        0UL // GlobalPointerTable
        0UL // TLSTable
        0UL // LoadConfigTable
        0UL // BoundImportTable
        0UL // TEMPORARY // ImportAddressTable
        0UL // DelayImportDescriptor

        if pe.CliHeader.IsSome then
            uint32 info.CliHeaderRva.Value // CliHeader
            uint32 SizeOf.CliHeader
        else 0UL

        0UL // Reserved

        for i = 0 to pe.SectionTable.Length - 1 do
            let section = info.Sections.Item(i)
            let header = section.Section.Header
            SectionName.toArray header.SectionName
            uint32 section.ActualSize.Value
            // TODO: Figure out how to calculate these fields from the data
            uint32 section.FileOffset.Value // VirtualAddress
            uint32 section.RoundedSize.Value
            uint32 section.FileOffset.Value // PointerToRawData
            header.PointerToRelocations
            0u // PointerToLineNumbers
            header.NumberOfRelocations
            0us // NumberOfLineNumbers
            uint32 header.Characteristics

        yield! empty (info.HeaderSizeRounded.Value - info.HeaderSizeActual.Value)
    }

// NOTE: This function won't work if more than one Cliheader is present, since the retrieved CliInfo will only be for the first one.
let private cli (pe: PEInfo) (header: CliHeader) =
    assert (Some header = pe.File.CliHeader)
    writer {
        let info = Option.get pe.CliHeader.Value
        uint32 SizeOf.CliHeader
        header.MajorRuntimeVersion
        header.MinorRuntimeVersion
        uint32 info.MetaDataRva.Value
        uint32 (info.MetaDataRootSize.Value + info.MetaDataStreamsSize.Value)
        uint32 header.Flags
        0u // EntryPointToken

        0u // RVA of Resources
        0u // Size of Resources

        if header.StrongNameSignature.IsEmpty
        then 0UL
        else
            uint32 info.StrongNameSignatureRva
            uint32 info.StrongNameSignatureSize

        0UL // CodeManagerTable
        0UL // VTableFixups // TODO: See if this needs to be assigned a value
        0UL // ExportAddressTableJumps
        0UL // ManagedNativeHeader

        header.StrongNameSignature

        // TODO: Write method bodies

        let root = header.Metadata
        CliSignature
        root.MajorVersion
        root.MinorVersion
        0u  // Reserved
        root.Version.Length
        info.MetaDataVersion.Value
        0us // Flags
        root.Streams.Count

        // TODO: Write stream headers.
        // NOTE: stream header offsets are relative to info.MetaDataRva
        // #~ stream header
        uint32 info.MetaDataTableOffset.Value
        uint32 info.MetaDataTableSize.Value
        MetadataTableStreamName

        // #~ stream
        let metadata = header.Metadata.Streams.Metadata
        0u // Reserved
        metadata.MajorVersion
        metadata.MinorVersion
        0uy // HeapSizes // TODO: Determine what value this should have.
        0uy // Reserved
        metadata.Valid
        0UL // Sorted // WHAT VALUE
        // Rows // TODO: Determine which integer in the array corresponds to which table.

        // Tables
        // bin.WriteU32 0 // Module

        // NOTE: Perhaps the Module table can be written after the fields of the #~ stream.

        // TODO: Write #Strings, #US, #GUID, and #Blob streams
    }

let private write pe (writer: PEInfo -> ByteWriter<_>) =
    let info = PEInfo pe
    let bin = new Writer<_> (writer info)

    headers info bin

    for section in info.Sections do
        let pos = bin.Position
        let fileOffset = section.FileOffset.Value

        if pos <> fileOffset then
            sprintf
                "The file offset indicating the start of the %A section (0x%X) did not match the current position of the writer (0x%X)"
                section.Section.Header.SectionName
                fileOffset
                pos
            |> invalidOp

        for data in section.Section.Data do
            match data with
            | RawData(Lazy bytes)-> bin.Write bytes
            | CliHeader header -> cli info header bin
            | ClrLoaderStub -> empty 8UL bin // TODO: Write the loader stub

        empty (section.RoundedSize.Value - section.ActualSize.Value) bin

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
                    try
                        Array.set bytes pos value
                    with
                    | ex ->
                        InvalidOperationException(sprintf "Array length is %i" bytes.Length, ex) |> raise
                override _.Write(ValidArrayIndex pos, source: byte[]) =
                    Array.Copy(source, 0, bytes, pos, source.Length) })
