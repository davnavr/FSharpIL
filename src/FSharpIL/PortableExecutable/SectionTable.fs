namespace FSharpIL.PortableExecutable

open System.Collections.Immutable
open System.ComponentModel

open FSharpIL.Metadata

type RawSectionData = unit -> byte[]

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
    | CntUninitializedData = 0x80u
    | MemDiscardable = 0x200_0000u
    | MemExecute = 0x2000_0000u
    | MemRead = 0x4000_0000u
    | MemWrite = 0x8000_0000u

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
    | TextSection
    // NOTE: According to the spec, this should be the last section of the PE file.
    | RelocSection
    | RsrcSection

type Section =
    { Data: ImmutableArray<SectionData>
      Kind: SectionKind }

    // TODO: How to cache this value?
    member this.Header =
        let name, flags =
            match this.Kind with
            | TextSection ->
                ".text"B, SectionFlags.CntCode ||| SectionFlags.MemExecute ||| SectionFlags.MemRead
            | RsrcSection ->
                ".rsrc"B, SectionFlags.CntInitializedData ||| SectionFlags.MemRead
            | RelocSection ->
                ".reloc"B, SectionFlags.CntInitializedData ||| SectionFlags.MemRead ||| SectionFlags.MemDiscardable
        { SectionName = SectionName.ofBytes name |> Option.get
          Data = this.Data
          PointerToRelocations = 0u
          NumberOfRelocations = 0us
          Characteristics = flags }

[<RequireQualifiedAccess>]
module SectionInfo =
    // II.25.2.3.3
    // NOTE: The RVA needs to be converted to/from file offset
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type DataDirectories =
        private
            { CliHeaderValue: Lazy<CliHeader option> }

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
        member this.CliHeader = this.CliHeaderValue.Value
        // Reserved

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type Info =
        private
            { Data: DataDirectories
              Sections: ImmutableArray<Section>
              TextSectionValue: Lazy<(int * Section) option> }

        static member Default =
            let sections =
                ImmutableArray.CreateRange [
                    let empty = RawData (fun() -> Array.replicate 8 0uy) |> ImmutableArray.Create // TEMPORARY
                    { Kind = TextSection
                      Data = ImmutableArray.Create(ClrLoaderStub, CliHeader CliHeader.Default) }
                    { Kind = RsrcSection; Data = empty }
                    { Kind = RelocSection; Data = empty }
                ]
            let data =
                { CliHeaderValue =
                    lazy
                        Seq.tryPick
                            (function
                            | { Kind = TextSection; Data = data } -> Some data
                            | _ -> None)
                            sections
                        |> Option.defaultValue ImmutableArray.Empty
                        |> Seq.tryPick
                            (function
                            | CliHeader header -> Some header
                            | _ -> None) }
            { Data = data
              Sections = sections
              TextSectionValue =
                lazy
                    let rec inner i =
                        if i >= sections.Length
                        then None
                        else
                           match sections.Item i with
                           | { Kind = TextSection } as text -> Some(i, text)
                           | _ -> None
                    inner 0 }

        member this.DataDirectories = this.Data
        member this.SectionTable = this.Sections

        member this.TextSection = this.TextSectionValue.Value

    // TODO: Create Computation Expression type for SectionTable.
    let addSection (section: SectionHeader) (info: Info) =
        invalidOp "bad"

type SectionInfo = SectionInfo.Info
type DataDirectories = SectionInfo.DataDirectories
