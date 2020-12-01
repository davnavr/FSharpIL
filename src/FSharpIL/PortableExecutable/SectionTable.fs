namespace FSharpIL.PortableExecutable

open System.Collections.Immutable
open System.ComponentModel

open FSharpIL.Metadata

type RawSectionData = Lazy<byte[]>

type SectionData =
    | RawData of RawSectionData
    // First 8 bytes of the .text section
    | ClrLoaderStub
    | CliHeader of CliHeader
    // TODO: Add cases for import table and import address table

[<System.Flags>]
type SectionFlags =
    | CntCode = 0x20u
    | CntInitializedData = 0x40u
    | CmtUninitializedData = 0x80u
    | MemExecute = 0x20000000u
    | MemRead = 0x40000000u
    | MemWrite = 0x80000000u

// II.25.3
type SectionHeader =
    { SectionName: SectionName
      // VirtualSize: uint32
      // VirtualAddress: uint32

      // NOTE: SizeOfRawData is "size of the initialized data on disk in bytes" and is a multiple of FileAlignment
      // TODO: PointerToRawData is file offset to the data, rounded up to multiple of file alignment
      Data: ImmutableArray<SectionData>

      /// Reserved value that should be set to zero.
      PointerToRelocations: uint32
      //PointerToLineNumbers: uint32
      /// Reserved value that should be set to zero.
      NumberOfRelocations: uint16
      //NumberOfLineNumbers: uint16
      Characteristics: SectionFlags }

type SectionKind =
    | TextSection of ImmutableArray<SectionData>
    // NOTE: According to the spec, this should be the last section of the PE file.
    | RelocSection of ImmutableArray<SectionData>
    | RsrcSection of ImmutableArray<SectionData>

    member this.Data =
        match this with
        | TextSection data
        | RsrcSection data -> data

    member this.Header =
        let name, flags, data =
            match this with
            | TextSection data ->
                ".text"B,
                SectionFlags.CntCode ||| SectionFlags.MemExecute ||| SectionFlags.MemRead,
                data
            | RsrcSection data ->
                ".rsrc"B,
                SectionFlags.CntInitializedData ||| SectionFlags.MemRead,
                data
        { SectionName = SectionName.ofBytes name |> Option.get
          Data = data
          PointerToRelocations = 0u
          NumberOfRelocations = 0us
          Characteristics = flags }

[<RequireQualifiedAccess>]
module SectionInfo =
    // II.25.2.3.3
    /// NOTE: The RVA needs to be converted to/from file offset
    [<EditorBrowsable(EditorBrowsableState.Never); RequireQualifiedAccess>]
    type Data =
        private { Sections: ImmutableArray<SectionKind> }
        // ExportTable
        member _.ImportTable = ()
        // ResourceTable
        // ExceptionTable
        // CertificateTable
        member _.BaseRelocationTable = () // TODO: This uses the Reloc section
        //DebugTable
        //CopyrightTable
        //GlobalPointer
        // TLSTable
        // LoadConfigTable
        // BoundImportTable
        member _.ImportAddressTable = ()
        // DelayImportDescriptor
        member this.CliHeader =
            this.Sections
            |> Seq.collect
                (function
                | TextSection data -> data
                | _ -> ImmutableArray.Empty)
            |> Seq.tryPick
                (function
                | CliHeader header -> Some header
                | _ -> None)
        // Reserved

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type Info =
        private
        | Sections of Data

        static member Default =
            let sections =
                ImmutableArray.CreateRange [
                    ImmutableArray.CreateRange [
                        ClrLoaderStub
                        CliHeader CliHeader.Default
                    ]
                    |> TextSection

                    RsrcSection ImmutableArray.Empty
                    RelocSection ImmutableArray.Empty
                ]
            Sections { Sections = sections }

        member this.DataDirectories = let (Sections data) = this in data
        member this.SectionTable = this.DataDirectories.Sections

    // TODO: Create Computation Expression type for SectionTable.
    let addSection (section: SectionHeader) (info: Info) =
        invalidOp "bad"

type SectionInfo = SectionInfo.Info
type DataDirectories = SectionInfo.Data
