namespace ILInfo

open System.Collections.Immutable

open FSharpIL
open FSharpIL.PortableExecutable
open FSharpIL.Reading

type WriteString = IndentedTextWriter -> (IndentedTextWriter -> unit) -> unit

[<NoComparison; NoEquality>]
type ILOutput =
    { Comment: WriteString
      Declaration: WriteString
      Keyword: WriteString
      StringLiteral: WriteString }

[<RequireQualifiedAccess>]
module ILOutput =
    let inline chars (str: string) (wr: IndentedTextWriter) = wr.Write str
    let comment ({ Comment = print }, wr) comment =
        print wr <| fun wr' ->
            wr'.Write "// "
            comment wr'
            wr'.WriteLine()
    let inline heading name (offset: FileOffset) out = comment out (fun wr -> fprintf wr "----- %s (%O)" name offset)
    let inline fieldf name size printer value out = comment out (fun wr -> fprintf wr "%s (%i bytes) = %a" name size printer value)
    let inline field name printer (value: 'Value) out = fieldf name sizeof<'Value> printer value out
    let inline newline (_, wr: IndentedTextWriter) = wr.WriteLine()

    /// Outputs IL code as text.
    let text =
        let print wr content = content wr
        { Comment = print
          Declaration = print
          Keyword = print
          StringLiteral = print }

    /// Outputs IL code as HTML.
    let html =
        //failwith "TODO: HTML output is not yet available"
        Unchecked.defaultof<ILOutput>

    [<RequireQualifiedAccess>]
    module Headers =
        let coffHeader header offset out =
            heading "COFF Header" offset out
            field "Machine" Print.enumeration header.Machine out
            field "NumberOfSections" Print.integer header.NumberOfSections out
            field "TimeDateStamp" Print.integer header.TimeDateStamp out
            field "SymbolTablePointer" Print.integer header.SymbolTablePointer out
            field "SymbolCount" Print.integer header.SymbolCount out
            field "OptionalHeaderSize" Print.integer header.OptionalHeaderSize out
            field "Characteristics" Print.bitfield header.Characteristics out
            newline out
            ValueSome out

        let private standardFieldsCommon fields out =
            field "Magic" Print.enumeration fields.Magic out
            field "MajorLinkerVersion" Print.integer fields.LMajor out
            field "MinorLinkerVersion" Print.integer fields.LMinor out
            field "SizeOfCode" Print.integer fields.CodeSize out
            field "SizeOfInitializedData" Print.integer fields.InitializedDataSize out
            field "SizeOfUninitializedData" Print.integer fields.UninitializedDataSize out
            field "EntryPointRva" Print.integer fields.EntryPointRva out
            field "BaseOfCode" Print.integer fields.BaseOfCode out

        let private ntSpecificFieldsCommon fields out =
            field "SectionAlignment" Print.integer fields.Alignment.SectionAlignment out
            field "FileAlignment" Print.integer fields.Alignment.FileAlignment out
            field "MajorOperatingSystemVersion" Print.integer fields.OSMajor out
            field "MinorOperatingSystemVersion" Print.integer fields.OSMinor out
            field "MajorImageVersion" Print.integer fields.UserMajor out
            field "MinorImageVersion" Print.integer fields.UserMinor out
            field "MajorSubsystemVersion" Print.integer fields.SubSysMajor out
            field "MinorSubsystemVersion" Print.integer fields.SubSysMinor out
            field "Win32VersionValue" Print.integer fields.Win32VersionValue out
            field "SizeOfImage" Print.integer fields.ImageSize out
            field "SizeOfHeaders" Print.integer fields.HeadersSize out
            field "FileChecksum" Print.integer fields.FileChecksum out
            field "Subsystem" Print.enumeration fields.Subsystem out
            field "DllCharacteristics" Print.bitfield fields.DllFlags out

        let inline private optionalHeaderImageBase { ImageBase = imageBase } out =
            field "ImageBase" Print.integer imageBase out

        let inline private ntSpecificFieldsSizes fields out =
            field "SizeOfStackReserve" Print.integer fields.StackReserveSize out
            field "SizeOfStackCommit" Print.integer fields.StackCommitSize out
            field "SizeOfHeapReserve" Print.integer fields.HeapReserveSize out
            field "SizeOfHeapCommit" Print.integer fields.HeapCommitSize out

        let private ntSpecificFieldsRemaining fields out =
            field "LoaderFlags" Print.integer fields.LoaderFlags out
            field "NumberOfDataDirectories" Print.integer fields.NumberOfDataDirectories out

        let inline private ntSpecificFields nt out =
            optionalHeaderImageBase nt out
            ntSpecificFieldsCommon nt out
            ntSpecificFieldsSizes nt out
            ntSpecificFieldsRemaining nt out

        let optionalHeader header offset out =
            match header with
            | ParsedOptionalHeader.PE32(std, nt) ->
                heading "PE32 Optional Header" offset out
                standardFieldsCommon std out
                field "BaseOfData" Print.integer std.BaseOfData out
                ntSpecificFields nt out
            | ParsedOptionalHeader.PE32Plus(std, nt) ->
                heading "PE32+ Optional Header" offset out
                standardFieldsCommon std out
                ntSpecificFields nt out
            newline out
            ValueSome out

        let private dataDirectoryNames =
            [|
                "Export Table"
                "Import Table"
                "Resource Table"
                "Exception Table"
                "Certificate Table"
                "Base Relocation Table"
                "Debug"
                "Copyright"
                "Global Pointer"
                "TLS Table"
                "Load Configuration Table"
                "Bound Import"
                "Import Address Table"
                "Delay Import Descriptor"
                "CLI Header"
                "Reserved"
            |]

        let dataDirectories ((_, directories): ParsedDataDirectories) offset out =
            heading "Data Directories" offset out
            for i = 0 to directories.Length - 1 do
                let name =
                    if i < dataDirectoryNames.Length
                    then sprintf "%s Table" dataDirectoryNames.[i]
                    else "Unknown"
                field name Print.rvaAndSize directories.[i] out
            newline out
            ValueSome out

        let sectionHeaders (headers: ParsedSectionHeaders) (offset: FileOffset) out =
            for i = 0 to headers.Length - 1 do
                let header = headers.[i]
                heading (sprintf "\"%O\" Section Header" header.SectionName) (offset + (uint32 i * Magic.sectionHeaderSize)) out
                fieldf "Name" 8u (fun wr () -> wr.Write header.SectionName) () out
                field "VirtualSize" Print.integer header.VirtualSize out
                field "VirtualAddress" Print.uint32 header.VirtualAddress out
                field "SizeOfRawData" Print.integer header.RawDataSize out
                field "PointerToRawData" Print.uint32 header.RawDataPointer out
                field "PointerToRelocations" Print.integer header.PointerToRelocations out
                field "PointerToLineNumbers" Print.integer header.PointerToLineNumbers out
                field "NumberOfRelocations" Print.integer header.NumberOfRelocations out
                field "NumberOfLineNumbers" Print.integer header.NumberOfLineNumbers out
                field "Characteristics" Print.bitfield header.Characteristics out
                newline out
            ValueSome out

        let cliHeader (header: ParsedCliHeader) offset out =
            heading "CLI Header" offset out
            field "Cb" Print.integer header.Cb out
            field "MajorRuntimeVersion" Print.integer header.MajorRuntimeVersion out
            field "MinorRuntimeVersion" Print.integer header.MinorRuntimeVersion out
            field "MetaData" Print.rvaAndSize header.Metadata out
            field "Flags" Print.bitfield header.Flags out
            field "EntryPointToken" Print.integer header.EntryPointToken out // TODO: Show what table the EntryPointToken refers to.
            field "Resources" Print.rvaAndSize header.Resources out
            field "StrongNameSignature" Print.rvaAndSize header.StrongNameSignature out
            field "CodeManagerTable" Print.rvaAndSize header.CodeManagerTable out
            field "VTableFixups" Print.rvaAndSize header.VTableFixups out
            field "ExportAddressTableJumps" Print.rvaAndSize header.ExportAddressTableJumps out
            field "ManagedNativeHeader" Print.rvaAndSize header.ManagedNativeHeader out
            //".corflags 0x%08X" (uint32 header.Flags)
            newline out
            ValueSome out

        let cliMetadataRoot (root: ParsedCliMetadataRoot) offset out =
            heading "CLI Metadata Root" offset out
            field "MajorVersion" Print.integer root.MajorVersion out
            field "MinorVersion" Print.integer root.MinorVersion out
            field "Reserved" Print.integer root.Reserved out
            field "Length" Print.integer root.Version.Length out
            fieldf "Version" root.Version.Length (fun wr () -> fprintf wr "\"%O\"" root.Version) () out
            field "Flags" Print.integer root.Flags out
            field "Streams" Print.integer root.Streams out
            newline out
            ValueSome out

        let metadataStreamHeaders (headers: ImmutableArray<ParsedStreamHeader>) offset out =
            for i = 0 to headers.Length - 1 do
                let header = &headers.ItemRef i
                let name = header.PrintedName
                heading (sprintf "\"%s\" Stream Header" name) offset out
                field "Offset" Print.integer (uint32 header.Offset) out
                field "Size" Print.integer header.Size out
                fieldf "Name" header.StreamName.Length (fun wr -> fprintf wr "\"%s\"") name out
                newline out
            ValueSome out

    let write includeFileHeaders includeCilMetadata vfilter =
        let inline header printer =
            match includeFileHeaders with
            | IncludeHeaders -> ValueSome printer
            | NoHeaders -> ValueNone
        let error state error offset out =
            eprintfn "error : %s" (ReadError.message state error offset)
            out
        { ReadLfanew = header (fun lfanew offset out ->
              heading "DOS Header" offset out
              field "lfanew" Print.integer (uint32 lfanew) out
              newline out
              ValueSome out)
          ReadCoffHeader = header Headers.coffHeader
          ReadOptionalHeader = header Headers.optionalHeader
          ReadDataDirectories = header Headers.dataDirectories
          ReadSectionHeaders = header Headers.sectionHeaders
          ReadCliMetadata =
            match includeCilMetadata with
            | IncludeMetadata ->
                { ReadCliHeader = header Headers.cliHeader
                  ReadMetadataRoot = header Headers.cliMetadataRoot
                  ReadStreamHeaders = header Headers.metadataStreamHeaders
                  // TODO: Read contents of streams if specified.
                  ReadStringsStream = ValueNone
                  ReadGuidStream = ValueNone
                  ReadUserStringStream = ValueNone
                  ReadBlobStream = ValueNone
                  ReadTables = ValueNone // TODO: Read tables.
                  HandleError = error }
                |> ValueSome
            | NoMetadata -> ValueNone
          HandleError = error }
