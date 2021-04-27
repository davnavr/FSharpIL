﻿namespace rec FSharpIL.PortableExecutable

open System.Collections.Immutable
open System.Diagnostics

open FSharpIL.Metadata

type SectionData =
    | RawData of (unit -> byte[])
    // First 8 bytes of the .text section
    | ClrLoaderStub
    | CliHeader of CliMetadata
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

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type SectionLocation =
    { VirtualSize: uint32
      VirtualAddress: uint32
      RawDataSize: uint32
      RawDataPointer: uint32 }

    member this.ContainsRva rva = rva >= this.VirtualAddress && rva < (this.VirtualAddress + this.VirtualSize)

/// (II.25.3)
type SectionHeader<'Data> =
    { SectionName: SectionName
      // VirtualSize: uint32
      // VirtualAddress: uint32

      // NOTE: SizeOfRawData is "size of the initialized data on disk in bytes" and is a multiple of FileAlignment
      // TODO: PointerToRawData is file offset to the data, rounded up to multiple of file alignment
      Data: 'Data

      /// Reserved value that should be set to zero.
      PointerToRelocations: uint32
      //PointerToLineNumbers: uint32
      /// Reserved value that should be set to zero.
      NumberOfRelocations: uint16
      //NumberOfLineNumbers: uint16
      Characteristics: SectionFlags }

// TODO: Should custom sections be allowed? It would mean that the section name would have to be checked when looking for the .text or .rsrc section.
[<StructuralComparison; StructuralEquality>]
type SectionKind =
    | TextSection
    // NOTE: According to the spec, this should be the last section of the PE file.
    | RelocSection
    | RsrcSection

[<DebuggerDisplay("{Data}", Name = "{Header.SectionName.ToString()}")>]
type Section =
    { Data: ImmutableArray<SectionData>
      Kind: SectionKind }

    // TODO: How to cache this value?
    member this.Header =
        let name, flags =
            match this.Kind with
            | TextSection ->
                SectionName.text, SectionFlags.CntCode ||| SectionFlags.MemExecute ||| SectionFlags.MemRead
            | RsrcSection ->
                SectionName.rsrc, SectionFlags.CntInitializedData ||| SectionFlags.MemRead
            | RelocSection ->
                SectionName.reloc, SectionFlags.CntInitializedData ||| SectionFlags.MemRead ||| SectionFlags.MemDiscardable
        { SectionName = name
          Data = this.Data
          PointerToRelocations = 0u
          NumberOfRelocations = 0us
          Characteristics = flags }

// II.25.2.3.3
[<Sealed>]
type DataDirectories internal(sections: PESections) =
    let cliHeader =
        lazy
            Option.bind
                (fun (sectionIndex, { Section.Data = data }) ->
                    Seq.indexed data
                    |> Seq.tryPick
                        (function
                        | (dataIndex, CliHeader header) ->
                            {| DataIndex = dataIndex
                               Header = header
                               SectionIndex = sectionIndex |}
                            |> Some
                        | _ -> None))
                sections.TextSection

    // ExportTable
    member _.ImportTable = ()
    // ResourceTable
    // ExceptionTable
    // CertificateTable
    member _.BaseRelocationTable = () // NOTE: This uses the Reloc section
    // DebugTable
    // CopyrightTable
    // GlobalPointer
    // TLSTable
    // LoadConfigTable
    // BoundImportTable
    member _.ImportAddressTable = ()
    // DelayImportDescriptor
    /// Returns the CLI header of the Portable Executable and the index of the section containing it.
    member _.CliHeader = cliHeader.Value
    // Reserved

/// Contains information about the sections of a Portable Executable file.
[<Sealed>]
type PESections(sections: ImmutableArray<Section>) as this =
    let rsrcSection = lazy this.FindSection RsrcSection
    let textSection = lazy this.FindSection TextSection

    member _.RsrcSection = rsrcSection.Value
    member _.TextSection = textSection.Value

    member val DataDirectories = DataDirectories this
    member _.SectionTable = sections

    member private _.FindSection (kind: SectionKind) =
        Seq.indexed sections
        |> Seq.tryPick
            (function
            | (i, section) when section.Kind = kind -> Some(i, section)
            | _ -> None)

    static member val Empty = PESections ImmutableArray.Empty
