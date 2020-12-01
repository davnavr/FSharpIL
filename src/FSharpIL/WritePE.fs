[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open System.IO

open FSharpIL.PortableExecutable
open FSharpIL.Utilities

[<AutoOpen>]
module private Helpers =
    [<RequireQualifiedAccess>]
    type State =
        | SectionHeader of int
        | SectionData of int
        | CliHeader of Metadata.CliHeader

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

type Builder internal() =
    member _.Combine(headers: PEHeaders, pe: PEFile) =
        { pe with Headers = headers }
    member _.Delay(f): PEFile = f()
    member _.Yield(headers: PEHeaders) = headers
    member _.Zero(): PEFile = PEFile.Default

let private write (file: PEFile) (bin: BinaryWriter) =
    bin.Write dosstub
    bin.Write pesig

    let coff = file.Headers.FileHeader
    Bytes.ofU16 coff.Machine |> bin.Write
    Bytes.ofU16 file.SectionTable.Length |> bin.Write
    Bytes.ofU32 coff.TimeDateStamp |> bin.Write
    Bytes.ofU32 coff.SymbolTablePointer |> bin.Write
    Bytes.ofU32 coff.SymbolCount |> bin.Write
    bin.Write [| 0xE0uy; 0uy |] // OptionalHeaderSize
    Bytes.ofU16 coff.Characteristics |> bin.Write

    let standard = file.Headers.StandardFields
    bin.Write [| 0xBuy; 1uy |] // Magic, 0x10B means PE32
    bin.Write standard.LMajor
    bin.Write standard.LMinor

    // TODO: In order to calculate the size of .text section, either write all sections to an array before writing the pe file
    // or write all sections first to a stream(s) that keeps track of length, before writing the headers afterward

    Bytes.empty 24 |> bin.Write // TEMPORARY
    // TODO: Figure out how to calculate the values for the rest of the standard fields
    // CodeSize
    // InitializedDataSize
    // UninitizalizedDataSize
    // EntryPointRva
    // BaseOfCode
    // BaseOfData

    let nt = file.Headers.NTSpecificFields
    Bytes.ofU32 nt.ImageBase |> bin.Write
    Bytes.ofU32 nt.Alignment.SectionAlignment |> bin.Write
    Bytes.ofU32 nt.Alignment.FileAlignment |> bin.Write
    Bytes.ofU16 nt.OSMajor |> bin.Write
    Bytes.ofU16 nt.OSMinor |> bin.Write
    Bytes.ofU16 nt.UserMajor |> bin.Write
    Bytes.ofU16 nt.UserMinor |> bin.Write
    Bytes.ofU16 nt.SubSysMajor |> bin.Write
    Bytes.ofU16 nt.SubSysMinor |> bin.Write
    Bytes.ofU32 nt.Win32VersionValue |> bin.Write
    Bytes.empty 4 |> bin.Write // TEMPORARY
    // ImageSize // TODO: Figure out how to calculate the ImageSize
    let hsizea, hsizer =
        let actual =
            uint dosstub.Length
            + uint pesig.Length
            + 20u // Coff
            + 24u // Standard PE32
            + 68u // NT
            + 128u // Data Directories, of which there are 16
            + (40u * uint32 file.SectionTable.Length)
        let falignment = nt.Alignment.FileAlignment
        // Rounds the actual size up to a factor of FileAlignment
        actual, falignment * ((actual + falignment - 1u) / falignment)
    Bytes.ofU32 hsizea |> bin.Write
    Bytes.ofU32 nt.FileChecksum |> bin.Write
    Bytes.ofU16 nt.Subsystem |> bin.Write
    Bytes.ofU16 nt.DllFlags |> bin.Write
    Bytes.ofU32 nt.StackReserveSize |> bin.Write
    Bytes.ofU32 nt.StackCommitSize |> bin.Write
    Bytes.ofU32 nt.HeapReserveSize |> bin.Write
    Bytes.ofU32 nt.HeapCommitSize |> bin.Write
    Bytes.ofU32 nt.LoaderFlags |> bin.Write
    Bytes.ofU32 0x10u |> bin.Write // NumberOfDataDirectories

    Bytes.empty 8 |> bin.Write // ExportTable
    // ImportTable
    Bytes.empty 24 |> bin.Write
    // BaseRelocationTable
    Bytes.empty 48 |> bin.Write
    // ImportAddressTable
    Bytes.empty 8 |> bin.Write // DelayImportDescriptor
    // CliHeader
    Bytes.empty 8 |> bin.Write // Reserved
    do // Padding
        int (hsizer - hsizea)
        |> Bytes.empty
        |> bin.Write
    // Section headers immediately follow the optional header

    let rec inner =
        function
        | State.SectionHeader index when file.SectionTable.Length < index ->
            let header = file.SectionTable.Item(index).Header
            SectionName.toArray header.SectionName |> bin.Write
            // TODO: Figure out how to calculate these from the data
            Bytes.empty 4 |> bin.Write // TEMPORARY // VirtualSize
            Bytes.empty 4 |> bin.Write // TEMPORARY // VirtualAddress
            Bytes.empty 4 |> bin.Write // TEMPORARY // SizeOfRawData
            Bytes.empty 4 |> bin.Write // TEMPORARY // PointerToRawData // Note that this is the file offset where the section starts, rounded to the nearest FileAlignment
            Bytes.ofU32 header.PointerToRelocations |> bin.Write
            Bytes.empty 4 |> bin.Write // PointerToLineNumbers
            Bytes.ofU32 header.NumberOfRelocations |> bin.Write
            Bytes.empty 4 |> bin.Write // NumberOfLineNumbers
            Bytes.ofU32 header.Characteristics |> bin.Write
            let state' = State.SectionHeader(index + 1)
            inner state'
        | State.SectionHeader _ ->
            let state' = State.SectionData 0
            inner state'
        | State.SectionData index when file.SectionTable.Length < index ->
            let section = file.SectionTable.Item(index)
            Seq.iter
                (function
                | RawData data -> bin.Write data.Value
                | ClrLoaderStub ->
                    // TODO: Write the 8 bytes of the loader stub
                    Bytes.empty 8 |> bin.Write // TEMPORARY
                | CliHeader cli ->
                    // TODO: Will this function still be tail recursive if a call to "inner State.CliHeader cli" is inserted here?
                    ())
                section.Data
            ()
        | State.CliHeader cli ->
            bin.Write [| 0x48uy; 0uy; 0uy; 0uy |] // HeaderSize
            ()
        | State.SectionData _ ->
            ()
    State.SectionHeader 0 |> inner

let toStream (stream: Stream) pe = // TODO: Use a fancy "state machine" when writing?
    use writer = new BinaryWriter(stream)
    write pe writer
