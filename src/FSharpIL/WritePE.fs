[<RequireQualifiedAccess>]
module FSharpIL.WritePE

open System.IO

open FSharpIL.PortableExecutable
open FSharpIL.Utilities

[<AutoOpen>]
module private Helpers =
    [<RequireQualifiedAccess>]
    type State =
        | FileHeaders
        | DataDirectories

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

let rec private write (file: PEFile) (bin: BinaryWriter) =
    function
    | State.FileHeaders ->
        bin.Write dosstub
        bin.Write pesig
        let coff = file.Headers.FileHeader
        Bytes.ofU16 coff.Machine |> bin.Write
        invalidOp "Number of sections"
        Bytes.ofU32 coff.TimeDateStamp |> bin.Write
        Bytes.ofU32 coff.SymbolTablePointer |> bin.Write
        Bytes.ofU32 coff.SymbolCount |> bin.Write
        invalidOp "Optional header size"
        Bytes.ofU16 coff.Characteristics |> bin.Write
        let standard = file.Headers.StandardFields
        bin.Write 0xBuy
        bin.Write 1uy
        bin.Write standard.LMajor
        bin.Write standard.LMinor
        invalidOp "rest of the standard fields"
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
        invalidOp "imagesize and headersize"
        // ImageSize
        // HeaderSize
        Bytes.ofU32 nt.FileChecksum |> bin.Write
        Bytes.ofU16 nt.Subsystem |> bin.Write
        Bytes.ofU16 nt.DllFlags |> bin.Write
        Bytes.ofU32 nt.StackReserveSize |> bin.Write
        Bytes.ofU32 nt.StackCommitSize |> bin.Write
        Bytes.ofU32 nt.HeapReserveSize |> bin.Write
        Bytes.ofU32 nt.HeapCommitSize |> bin.Write
        Bytes.ofU32 nt.LoaderFlags |> bin.Write
        bin.Write [| 16uy; 0uy; 0uy; 0uy |] // NumberOfDataDirectories
        write file bin State.DataDirectories
    | State.DataDirectories ->
        ()

let toStream (stream: Stream) pe = // TODO: Use a fancy "state machine" when writing?
    use writer = new BinaryWriter(stream)
    write pe writer State.FileHeaders
