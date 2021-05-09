[<RequireQualifiedAccess>]
module ILInfo.ILOutput

open System.IO
open System.Text

open Microsoft.FSharp.Core.Printf

open FSharpIL.Metadata
open FSharpIL.PortableExecutable
open FSharpIL.Reading

let inline heading name (offset: FileOffset) wr = fprintfn wr "// ----- %s (0x%016X)" name (uint64 offset)

let inline fieldf name size printer wr value = fprintfn wr "// %s (%i bytes) = %a" name size printer value

let inline field name printer wr (value: 'Value) = fieldf name sizeof<'Value> printer wr value

let inline optional printer wr value  = ValueOption.iter (printer wr) value

// TODO: Format values nicely

let coffHeader header offset wr =
    heading "COFF Header" offset wr
    field "Machine" Print.enumeration wr header.Machine
    field "NumberOfSections" Print.integer wr header.NumberOfSections
    field "TimeDateStamp" Print.integer wr header.TimeDateStamp
    field "SymbolTablePointer" Print.integer wr header.SymbolTablePointer
    field "SymbolCount" Print.integer wr header.SymbolCount
    field "OptionalHeaderSize" Print.integer wr header.OptionalHeaderSize
    field "Characteristics" Print.bitfield wr header.Characteristics
    wr

let standardFields fields offset wr =
    heading "Standard Fields" offset wr
    field "Magic" Print.enumeration wr fields.Magic
    field "MajorLinkerVersion" Print.integer wr fields.LMajor
    field "MinorLinkerVersion" Print.integer wr fields.LMinor
    field "SizeOfCode" Print.integer wr fields.CodeSize
    field "SizeOfInitializedData" Print.integer wr fields.InitializedDataSize
    field "SizeOfUninitializedData" Print.integer wr fields.UninitializedDataSize
    field "EntryPointRva" Print.integer wr fields.EntryPointRva
    field "BaseOfCode" Print.integer wr fields.BaseOfCode
    optional (field "BaseOfData" Print.integer) wr fields.BaseOfData
    wr

let ntSpecificFields fields offset wr =
    let (salignment, falignment) = fields.Alignment
    heading "NT Specific Fields" offset wr
    // TODO: Write different size fields if PE32+
    field "ImageBase" Print.integer wr (uint32 fields.ImageBase) // NOTE: This is 8 bytes long instead for PE32+
    field "SectionAlignment" Print.integer wr salignment
    field "FileAlignment" Print.integer wr falignment
    field "MajorOperatingSystemVersion" Print.integer wr fields.OSMajor
    field "MinorOperatingSystemVersion" Print.integer wr fields.OSMinor
    field "MajorImageVersion" Print.integer wr fields.UserMajor
    field "MinorImageVersion" Print.integer wr fields.UserMinor
    field "MajorSubsystemVersion" Print.integer wr fields.SubSysMajor
    field "MinorSubsystemVersion" Print.integer wr fields.SubSysMinor
    field "Win32VersionValue" Print.integer wr fields.Win32VersionValue
    field "SizeOfImage" Print.integer wr fields.ImageSize
    field "SizeOfHeaders" Print.integer wr fields.HeadersSize
    field "FileChecksum" Print.integer wr fields.FileChecksum
    field "Subsystem" Print.enumeration wr fields.Subsystem
    field "DllCharacteristics" Print.bitfield wr fields.DllFlags
    field "SizeOfStackReserve" Print.integer wr (uint32 fields.StackReserveSize) // NOTE: This is 8 bytes long instead for PE32+
    field "SizeOfStackCommit" Print.integer wr (uint32 fields.StackCommitSize) // NOTE: This is 8 bytes long instead for PE32+
    field "SizeOfHeapReserve" Print.integer wr (uint32 fields.HeapReserveSize) // NOTE: This is 8 bytes long instead for PE32+
    field "SizeOfHeapCommit" Print.integer wr (uint32 fields.HeapCommitSize) // NOTE: This is 8 bytes long instead for PE32+
    field "LoaderFlags" Print.integer wr fields.LoaderFlags
    field "NumberOfDataDirectories" Print.integer wr fields.NumberOfDataDirectories
    wr

let dataDirectoryNames =
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

let dataDirectories (directories: ParsedDataDirectories) offset wr =
    heading "Data Directories" offset wr
    for i = 0 to directories.Length - 1 do
        let name =
            if i < dataDirectoryNames.Length
            then sprintf "%s Table" dataDirectoryNames.[i]
            else "Unknown"
        field name Print.rvaAndSize wr directories.[i]
    wr

let sectionHeaders (headers: ParsedSectionHeaders) (offset: FileOffset) wr =
    for i = 0 to headers.Length - 1 do
        let header = headers.[i]
        heading (sprintf "\"%O\" Section Header" header.SectionName) (offset + (uint64 i * 40UL)) wr
        field "Name" (fun wr () -> wr.Write header.SectionName) wr () // TODO: Make sure length includes padding // TODO: Fix length
        field "VirtualSize" Print.integer wr header.Data.VirtualSize
        field "VirtualAddress" Print.integer wr header.Data.VirtualAddress
        field "SizeOfRawData" Print.integer wr header.Data.RawDataSize
        field "PointerToRawData" Print.integer wr header.Data.RawDataPointer
        field "PointerToRelocations" Print.integer wr header.PointerToRelocations
        field "PointerToLineNumbers" Print.integer wr header.PointerToLineNumbers
        field "NumberOfRelocations" Print.integer wr header.NumberOfRelocations
        field "NumberOfLineNumbers" Print.integer wr header.NumberOfLineNumbers
        field "Characteristics" Print.bitfield wr header.Characteristics
    wr

let cliHeader (header: ParsedCliHeader) offset wr =
    heading "CLI Header" offset wr
    field "Cb" Print.integer wr header.Size
    field "MajorRuntimeVersion" Print.integer wr header.MajorRuntimeVersion
    field "MinorRuntimeVersion" Print.integer wr header.MinorRuntimeVersion
    field "MetaData" Print.rvaAndSize wr header.MetaData
    field "Flags" Print.bitfield wr header.Flags
    field "EntryPointToken" Print.integer wr header.EntryPointToken // TODO: Show what table the EntryPointToken refers to.
    field "Resources" Print.rvaAndSize wr header.Resources
    field "StrongNameSignature" Print.integer wr header.StrongNameSignature
    field "CodeManagerTable" Print.integer wr header.CodeManagerTable
    field "VTableFixups" Print.rvaAndSize wr header.VTableFixups
    field "ExportAddressTableJumps" Print.integer wr header.ExportAddressTableJumps
    field "ManagedNativeHeader" Print.integer wr header.ManagedNativeHeader
    wr

let metadataRoot (root: ParsedMetadataRoot) offset wr =
    heading "CLI Metadata Root" offset wr
    field "MajorVersion" Print.integer wr root.MajorVersion
    field "MinorVersion" Print.integer wr root.MinorVersion
    field "Reserved" Print.integer wr root.Reserved
    field "Length" Print.integer wr root.Version.Length
    // TODO: Make sure length includes padding
    fieldf "Version" root.Version.Length (fun wr () -> fprintf wr "\"%O\"" root.Version) wr ()
    field "Flags" Print.integer wr root.Flags
    field "Streams" Print.integer wr root.Streams
    wr

let streamHeader (header: ParsedStreamHeader) _ _ wr =
    field "Offset" Print.integer wr header.Offset
    field "Size" Print.integer wr header.Size
    fieldf
        "StreamName"
        header.Name.Length
        (fun wr () ->
            let name = header.Name.AsSpan()
            fprintf wr "\"%s\"" (Encoding.ASCII.GetString name))
        wr
        ()
    wr

let metadataTablesHeader header offset wr =
    let rec rows wr flags =
        match flags with
        | MetadataTableFlags.None -> ()
        | _ ->
            let flags' = flags >>> 1
            match header.Rows.TryGetValue(flags &&& MetadataTableFlags.Module) with
            | true, count -> fprintf wr "0x%08X, " count
            | false, _ -> ()
            rows wr flags'
    heading "Metadata Tables Header" offset wr
    field "Reserved" Print.integer wr header.Reserved1
    field "MajorVersion" Print.integer wr header.MajorVersion
    field "MinorVersion" Print.integer wr header.MinorVersion
    field "HeapSizes" Print.bitfield wr header.HeapSizes
    field "Reserved" Print.integer wr header.Reserved2
    field "Valid" Print.bitfield wr header.Valid
    field "Sorted" Print.bitfield wr header.Sorted
    fieldf
        "Rows"
        (4 * header.Rows.Count)
        (fun wr () ->
            wr.Write '['
            rows wr header.Valid
            wr.Write ']')
        wr
        ()

let moduleTable (tables: ParsedMetadataTables) strings guid (wr: TextWriter) =
    wr.WriteLine()
    wr.WriteLine "// Module (0x00)"
    // Module
    for i = 0 to int32 tables.Module.Size do
        let row = tables.Module.[i]
        let inline fguid name id = fieldf name tables.Header.HeapSizes.GuidSize (Print.guid guid) wr id
        field "Generation" Print.integer wr row.Generation
        wr.Write ".module "
        Print.identifier strings wr row.Name
        fprintfn wr "// %O" row.Name
        fguid "Mvid" row.Mvid
        fguid "GenerationId" row.EncId
        fguid "BaseGenerationId" row.EncBaseId

let metadataTables headers il strings guid (tables: ParsedMetadataTables) offset wr =
    match headers with
    | NoHeaders -> ()
    | IncludeHeaders ->
        metadataTablesHeader tables.Header offset wr
    match il, strings, guid with
    | _, ValueNone, _ -> eprintfn "error : \"#Strings\" metadata heap is missing."
    | _, _, ValueNone -> eprintfn "error : \"#GUID\" metadata heap is missing."
    | NoIL, _, _ -> ()
    | IncludeIL, ValueSome strings', ValueSome guid' ->
        moduleTable tables strings' guid' wr
    wr

let handleError state error offset wr =
    eprintfn "error : %s" (ReadError.message state error offset)
    wr

let write headers il =
    let inline header printer =
        match headers with
        | IncludeHeaders -> ValueSome printer
        | NoHeaders -> ValueNone
    { MetadataReader.empty with
       ReadCoffHeader = header coffHeader
       ReadStandardFields = header standardFields
       ReadNTSpecificFields = header ntSpecificFields
       ReadDataDirectories = header dataDirectories
       ReadSectionHeaders = header sectionHeaders
       ReadCliHeader = header cliHeader
       ReadMetadataRoot = header metadataRoot
       ReadStreamHeader = header streamHeader
       //ReadString
       ReadMetadataTables = ValueSome(metadataTables headers il)
       HandleError = handleError }
