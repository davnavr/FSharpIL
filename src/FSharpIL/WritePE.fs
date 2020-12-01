[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open System.IO

open FSharpIL.PortableExecutable

type Builder internal() =
    member _.Combine(headers: PEHeaders, pe: PEFile) =
        { pe with Headers = headers }
    member _.Delay(f): PEFile = f()
    member _.Yield(headers: PEHeaders) = headers
    member _.Zero(): PEFile = PEFile.Default

[<AutoOpen>]
module private Helpers =
    [<RequireQualifiedAccess>]
    type State =
        | SectionHeader of int
        | SectionData of int
        | CliHeader of Metadata.CliHeader

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

type private Writer(stream: Stream) =
    let writer = new BinaryWriter(stream)

    member _.Write(bytes: byte[]) = writer.Write bytes

    member _.WriteU8(byte: byte) = writer.Write byte

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

    member _.SeekStart() = writer.Seek(0, SeekOrigin.Begin) |> ignore

    interface System.IDisposable with
        member _.Dispose() = writer.Dispose()

let private write (file: PEFile) (bin: Writer) =
    let rec inner =
        function
        | State.SectionHeader index when file.SectionTable.Length < index ->
            let header = file.SectionTable.Item(index).Header
            SectionName.toArray header.SectionName |> bin.Write
            // TODO: Figure out how to calculate these from the data
            bin.WriteEmpty 4 // TEMPORARY // VirtualSize
            bin.WriteEmpty 4 // TEMPORARY // VirtualAddress
            bin.WriteEmpty 4 // TEMPORARY // SizeOfRawData
            bin.WriteEmpty 4 // TEMPORARY // PointerToRawData // Note that this is the file offset where the section starts, rounded to the nearest FileAlignment
            bin.WriteU32 header.PointerToRelocations
            bin.WriteEmpty 4 // PointerToLineNumbers
            bin.WriteU16 header.NumberOfRelocations
            bin.WriteEmpty 2 // NumberOfLineNumbers
            bin.WriteU32 header.Characteristics
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
                    bin.WriteEmpty 8 // TEMPORARY
                | CliHeader cli ->
                    // TODO: Will this function still be tail recursive if a call to "inner State.CliHeader cli" is inserted here?
                    ())
                section.Data
            ()
        | State.CliHeader cli ->
            bin.WriteU32 0x48u // HeaderSize
            ()
        | State.SectionData _ ->
            ()

    State.SectionHeader 0 |> inner

    bin.SeekStart()
    bin.Write dosstub
    bin.Write pesig

    let coff = file.Headers.FileHeader
    bin.WriteU16 coff.Machine
    bin.WriteU16 file.SectionTable.Length
    bin.WriteU32 coff.TimeDateStamp
    bin.WriteU32 coff.SymbolTablePointer
    bin.WriteU32 coff.SymbolCount
    bin.WriteU16 0xE0us // OptionalHeaderSize
    bin.WriteU16 coff.Characteristics

    let standard = file.Headers.StandardFields
    bin.WriteU16 0x10Bus // Magic, 0x10B means PE32
    bin.WriteU8 standard.LMajor
    bin.WriteU8 standard.LMinor

    // TODO: In order to calculate the size of .text section, either write all sections to an array before writing the pe file
    // or write all sections first to a stream(s) that keeps track of length, before writing the headers afterward.
    // The latter can be accomplished by using a stream that supports seeking and by going to the beginning after all sections are written

    bin.WriteEmpty 24 // TEMPORARY
    // TODO: Figure out how to calculate the values for the rest of the standard fields
    // CodeSize
    // InitializedDataSize
    // UninitizalizedDataSize
    // EntryPointRva
    // BaseOfCode
    // BaseOfData

    let nt = file.Headers.NTSpecificFields
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
    bin.WriteEmpty 4 // TEMPORARY
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
    bin.WriteU32 hsizea
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
    // ImportTable
    bin.WriteEmpty 24
    // BaseRelocationTable
    bin.WriteEmpty 48
    // ImportAddressTable
    bin.WriteEmpty 8 // DelayImportDescriptor
    // CliHeader
    bin.WriteEmpty 8// Reserved
    int (hsizer - hsizea) |> bin.WriteEmpty // Padding
    // Section headers immediately follow the optional header

let toStream (ValidStream stream) pe =
    use writer = new Writer(stream)
    write pe writer
