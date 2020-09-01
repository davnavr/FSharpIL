namespace FSharpIL.PortableExecutable

open System.Collections.Immutable
open System.ComponentModel

open FSharpIL.Metadata

// II.25.2.3.3
/// NOTE: The RVA needs to be converted to/from file offset
type DataDirectories =
    { // ExportTable
      ImportTable: unit
      // ResourceTable
      // ExceptionTable
      // CertificateTable
      BaseRelocationTable: unit
      //DebugTable
      //CopyrightTable
      //GlobalPointer
      // TLSTable
      // LoadConfigTable
      // BoundImportTable
      ImportAddressTable: unit
      // DelayImportDescriptor
      CliHeader: CliHeader option
      // Reserved
      }

type RawSectionData = Lazy<byte[]>

type SectionData =
    | RawData of RawSectionData
    | CliHeader of CliHeader // NOTE: First 8 bytes of .text section is usually used by CLR loader stub
    // TODO: Add cases for import table and import address table
    // NOTE: Some data have rules about where it can be placed, such as the Vtable fixup (II.25.3.3.3) needing to be in a read-write section

[<System.Flags>]
type SectionFlags =
    | Code = 0x20u
    | InitializedData = 0x40u
    | UninitializedData = 0x80u
    | Execute = 0x20000000u
    | Read = 0x40000000u
    | Write = 0x80000000u

// II.25.3
/// NOTE: Section headers begin after the file headers, but must account for SizeOfHeaders, which is rounded up to a multiple of FileAlignment.
type SectionHeader =
    { SectionName: SectionName
      // VirtualSize: uint32
      // VirtualAddress: uint32
      Data: ImmutableArray<SectionData>
      //PointerToRelocations: uint32
      //PointerToLineNumbers: uint32
      //NumberOfRelocations: uint16
      //NumberOfLineNumbers: uint16
      Characteristics: SectionFlags
      }

[<RequireQualifiedAccess>]
module SectionInfo =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    type Info =
        private
            { Data: DataDirectories
              Sections: ImmutableArray<SectionHeader> }

        static member Default =
            { Data =
                { ImportTable = ()
                  BaseRelocationTable = ()
                  ImportAddressTable = ()
                  CliHeader = Some CliHeader.Default }
              Sections =
                invalidOp "add default sections" }

        member this.DataDirectories = this.Data
        member this.SectionTable = this.Sections

    let addSection (section: SectionHeader) (info: Info) =
        let rec validate index state =
            let next = validate (index + 1)
            if index >= section.Data.Length then
                Ok { state with Sections = state.Sections.Add section }
            else
                match section.Data.Item index with // TODO: Check if this is this tail recursive.
                | RawData _ -> next state
                | CliHeader cli when state.Data.CliHeader.IsNone ->
                    next { state with Data = { state.Data with CliHeader = Some cli } }
                | data -> Error data
        validate 0 info

type SectionInfo = SectionInfo.Info
