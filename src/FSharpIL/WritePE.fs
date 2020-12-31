[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open FSharp.Core.Operators.Checked

open System
open System.Collections.Generic

open FSharpIL.Bytes
open FSharpIL.Magic
open FSharpIL.PortableExecutable

[<RequireQualifiedAccess>]
module Size =
    let PEHeader = DosStub.Length + PESignature.Length |> uint64

    [<Literal>]
    let CoffHeader = 20UL

    /// The value of the `SizeOfOptionalHeader` field in the COFF file header.
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

type SectionInfo =
    { ActualSize: uint64
      DataSizes: uint64[]
      FileOffset: uint64
      /// The size of the section rounded up to a multiple of FileAlignment
      RoundedSize: uint64
      Section: Section }

/// Stores the sizes and file offsets of the various objects and structs that make up a PE file.
[<Sealed>]
type PEInfo (pe: PEFile) =
    let mutable codeSize = 0UL
    let mutable initializedDataSize = 0UL
    let mutable uninitializedDataSize = 0UL
    let headerSizeActual =
        Size.PEHeader
        + Size.CoffHeader
        + Size.StandardFields
        + Size.NTSpecificFields
        + Size.DataDirectories
        + (uint64 pe.SectionTable.Length * Size.SectionHeader)
    let headerSizeRounded =
        Round.upTo
            (uint64 pe.NTSpecificFields.Alignment.FileAlignment)
            headerSizeActual
    let cliMetadata = Dictionary<int * int, WriteCli.CliInfo> 1
    let sections = Array.zeroCreate<SectionInfo> pe.SectionTable.Length

    do // Calculate section size and location information.
        for sectionIndex = 0 to pe.SectionTable.Length - 1 do
            let section = pe.SectionTable.Item sectionIndex
            let fileOffset =
                if sectionIndex = 0
                then headerSizeRounded
                else
                    let prevSection = Array.get sections (sectionIndex - 1)
                    prevSection.FileOffset + prevSection.RoundedSize
            let dataSizes =
                let mutable pos = fileOffset
                Array.init // TODO: Figure out if this is optimized to be a for loop.
                    section.Data.Length
                    (fun itemIndex ->
                        let item = section.Data.Item itemIndex
                        let size =
                            match item with
                            | CliHeader cli ->
                                let info = WriteCli.CliInfo(cli, pos)
                                cliMetadata.Item <- (sectionIndex, itemIndex), info
                                info.TotalLength
                            | ClrLoaderStub -> 8UL
                            | RawData(Lazy data) -> uint64 data.Length
                        pos <- pos + size
                        size)
            let actualSize = Array.sum dataSizes
            { ActualSize = actualSize
              DataSizes = dataSizes
              FileOffset = fileOffset
              RoundedSize = Round.upTo (uint64 pe.NTSpecificFields.Alignment.FileAlignment) actualSize 
              Section = section }
            |> Array.set sections sectionIndex

    do // Calculates the values for sizes in the standard fields (II.25.2.3.1)
        for { RoundedSize = rsize; Section = section } in sections do
            let inline hasFlag flag = section.Header.Characteristics.HasFlag flag

            if hasFlag SectionFlags.CntCode then
                codeSize <- codeSize + rsize

            if hasFlag SectionFlags.CntInitializedData then
                initializedDataSize <- initializedDataSize + rsize

            if hasFlag SectionFlags.CntUninitializedData then
                uninitializedDataSize <- uninitializedDataSize + rsize

    let totalLength =
        let sections =
            Array.sumBy
                (fun { RoundedSize = rsize } -> rsize)
                sections
        headerSizeRounded + sections

    member _.File = pe

    member _.CodeSize = codeSize
    member _.InitializedDataSize = initializedDataSize
    member _.UninitializedDataSize = uninitializedDataSize

    /// Gets the size of the headers rounded up to nearest multiple of FileAlignment.
    member _.HeaderSizeActual = headerSizeActual
    member _.HeaderSizeRounded = headerSizeRounded

    member _.Sections = sections

    member val CliHeaderRva =
        match pe.DataDirectories.CliHeader with
        | Some header ->
            let section = Array.get sections header.SectionIndex
            let mutable offset = section.FileOffset
            for i = 0 to header.DataIndex - 1 do
                offset <- offset + (Array.get section.DataSizes i)
            offset
        | None -> 0UL

    /// Gets the total length of the PE file.
    member _.TotalLength = totalLength

/// Writes the DOS stub and PE signature of a PE file.
// II.25.2.1
let peHeader() =
    bytes {
        DosStub
        PESignature
    }
    |> withLength Size.PEHeader

// II.25.2.2
let coffHeader (pe: PEFile) =
    bytes {
        let coff = pe.FileHeader
        uint16 coff.Machine
        uint16 pe.SectionTable.Length
        coff.TimeDateStamp
        coff.SymbolTablePointer
        coff.SymbolCount
        Size.OptionalHeader
        uint16 coff.Characteristics
    }
    |> withLength Size.CoffHeader

// II.25.2.3.1
let standardFields (info: PEInfo) =
    let sectionRva =
        function
        | Some(i, _) -> (Array.get info.Sections i).FileOffset
        | None -> 0UL
    bytes {
        let pe = info.File
        let standard = pe.StandardFields
        PE32
        standard.LMajor
        standard.LMinor
        uint32 info.CodeSize
        uint32 info.InitializedDataSize
        uint32 info.UninitializedDataSize
        // NOTE: The EntryPointRva always has a value regardless of whether or not it is a .dll or .exe, and points to somewhere special (see the end of II.25.2.3.1)
        0u // EntryPointRva // TODO: Figure out what this value should be.
        sectionRva pe.Sections.TextSection |> uint32 // BaseOfCode, matches the RVA of the .text section
        sectionRva pe.Sections.TextSection |> uint32 // BaseOfData, matches the RVA of the .rsrc section
    }
    |> withLength Size.StandardFields

// II.25.2.3.2
let ntSpecificFields (info: PEInfo) =
    bytes {
        let nt = info.File.NTSpecificFields
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
        uint32 info.HeaderSizeRounded
        nt.FileChecksum
        uint16 nt.Subsystem
        uint16 nt.DllFlags
        nt.StackReserveSize
        nt.StackCommitSize
        nt.HeapReserveSize
        nt.HeapCommitSize
        nt.LoaderFlags
        0x10u // NumberOfDataDirectories
    }
    |> withLength Size.NTSpecificFields

// II.25.2.3.3
let dataDirectories (info: PEInfo) =
    bytes {
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

        // CliHeader
        if info.File.CliHeader.IsSome then
            uint32 info.CliHeaderRva
            uint32 WriteCli.Size.CliHeader
        else 0UL

        0UL // Reserved
    }
    |> withLength Size.DataDirectories

let headers (info: PEInfo) =
    bytes {
        peHeader()
        coffHeader info.File
        standardFields info 
        ntSpecificFields info
        dataDirectories info

        // Section headers
        for i = 0 to info.File.SectionTable.Length - 1 do
            let section = Array.get info.Sections i
            let header = section.Section.Header
            SectionName.toArray header.SectionName
            uint32 section.ActualSize
            // TODO: Figure out how to calculate these fields from the data
            uint32 section.FileOffset // VirtualAddress
            uint32 section.RoundedSize
            uint32 section.FileOffset // PointerToRawData
            header.PointerToRelocations
            0u // PointerToLineNumbers
            header.NumberOfRelocations
            0us // NumberOfLineNumbers
            uint32 header.Characteristics

        empty (info.HeaderSizeRounded - info.HeaderSizeActual)
    }
    |> withLength info.HeaderSizeRounded

let write pe (writer: PEInfo -> ByteWriter<_>) =
    let info = PEInfo pe
    let writer' = new Writer<_> (writer info)

    headers info writer'

    // TODO: Write streams

    withLength info.TotalLength ignore writer'
    writer'.GetResult()

let toArray pe =
    write
        pe
        (fun info ->
            let (|ValidArrayIndex|) (i: uint64) =
                if i > uint64 Int32.MaxValue 
                then invalidArg "pe" "The PortableExecutable file is too large to fit inside of a byte array of size."
                else int32 i
            let (ValidArrayIndex totalLength) = info.TotalLength
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
