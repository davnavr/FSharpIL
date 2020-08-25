/// Low-level types representing the content of a Portable Executable file.
module FSharpIL.PETypes

open FSharpIL.Utilities

type MsDosStub =
    | MsDosStub of byte[] // TODO: Should immutable collections be used here?

    static member Default =
        [|
            0x0E; 0x1F; 0xBA; 0x0E; 0x00; 0xB4; 0x09; 0xCD;
            0x21; 0xB8; 0x01; 0x4C; 0xCD; 0x21; 0x54; 0x68;
            0x69; 0x73; 0x20; 0x70; 0x72; 0x6F; 0x67; 0x72;
            0x61; 0x6D; 0x20; 0x63; 0x61; 0x6E; 0x6E; 0x6F;
            0x74; 0x20; 0x62; 0x65; 0x20; 0x72; 0x75; 0x6e;
            0x20; 0x69; 0x6E; 0x20; 0x44; 0x4F; 0x53; 0x20;
            0x6D; 0x6F; 0x64; 0x65; 0x2e; 0x0d; 0x0D; 0x0A;
            0x24; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00;
        |]
        |> Array.map byte
        |> MsDosStub

type IsDll =
    | Dll
    | Exe

// II.25.2.2.1
[<System.Flags>]
type ImageFileFlags =
    | FileRelocsStripped = 0x0001us
    | FileExecutableImage = 0x0002us
    | File32BitMachine = 0x0100us
    | FileDll = 0x2000us

[<RequireQualifiedAccess>]
type FileCharacteristics =
    | Valid of IsDll
    | Custom of ImageFileFlags

    member this.Flags =
        match this with
        | FileCharacteristics.Valid dll ->
            let isDll =
                match dll with
                | Dll -> ImageFileFlags.FileDll
                | Exe -> Enum.from 0us
            ImageFileFlags.FileExecutableImage ||| isDll
        | FileCharacteristics.Custom value -> value

type PEFileHeader =
    { Machine: uint16
      TimeDateStamp: uint32
      SymbolTablePointer: uint32
      SymbolCount: uint32
      Characteristics: FileCharacteristics }

    static member Default =
        { Machine = 0x14Cus
          TimeDateStamp = 0u
          SymbolTablePointer = 0u
          SymbolCount = 0u
          Characteristics = FileCharacteristics.Valid Dll }

type PortableExecutable =
    { MsDosStub: MsDosStub
      PEFileHeader: PEFileHeader }

    static member Empty =
        { MsDosStub = MsDosStub.Default
          PEFileHeader = PEFileHeader.Default }
