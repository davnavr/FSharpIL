[<RequireQualifiedAccess>]
module ILInfo.ILOutput

open System.Text

open Microsoft.FSharp.Core.Printf

open FSharpIL.Metadata
open FSharpIL.PortableExecutable
open FSharpIL.Reading

let inline heading name (offset: FileOffset) wr =
    fprintfn wr "// ----- %s (0x%016X)" name (uint64 offset)
    wr

let inline fieldf name size printer value wr =
    fprintfn wr "// %s (%i bytes) = %a" name size printer value
    wr

let inline field name printer (value: 'Value) wr = fieldf name sizeof<'Value> printer value wr

let optional printer value wr =
    match value with
    | ValueSome value' -> printer value' wr
    | ValueNone -> wr

// TODO: Format values nicely

let coffHeader header offset =
    heading "COFF Header" offset
    >> field "Machine" Print.enumeration header.Machine
    >> field "NumberOfSections" Print.integer header.NumberOfSections
    >> field "TimeDateStamp" Print.integer header.TimeDateStamp
    >> field "SymbolTablePointer" Print.integer header.SymbolTablePointer
    >> field "SymbolCount" Print.integer header.SymbolCount
    >> field "OptionalHeaderSize" Print.integer header.OptionalHeaderSize
    >> field "Characteristics" Print.bitfield header.Characteristics

let standardFields fields offset =
    heading "Standard Fields" offset
    >> field "Magic" Print.enumeration fields.Magic
    >> field "MajorLinkerVersion" Print.integer fields.LMajor
    >> field "MinorLinkerVersion" Print.integer fields.LMinor
    >> field "SizeOfCode" Print.integer fields.CodeSize
    >> field "SizeOfInitializedData" Print.integer fields.InitializedDataSize
    >> field "SizeOfUninitializedData" Print.integer fields.UninitializedDataSize
    >> field "EntryPointRva" Print.integer fields.EntryPointRva
    >> field "BaseOfCode" Print.integer fields.BaseOfCode
    >> optional (field "BaseOfData" Print.integer) fields.BaseOfData

let ntSpecificFields fields offset =
    let (salignment, falignment) = fields.Alignment
    heading "NT Specific Fields" offset
    // TODO: Write different size fields if PE32+
    >> field "ImageBase" Print.integer (uint32 fields.ImageBase) // NOTE: This is 8 bytes long instead for PE32+
    >> field "SectionAlignment" Print.integer salignment
    >> field "FileAlignment" Print.integer falignment
    >> field "MajorOperatingSystemVersion" Print.integer fields.OSMajor
    >> field "MinorOperatingSystemVersion" Print.integer fields.OSMinor
    >> field "MajorImageVersion" Print.integer fields.UserMajor
    >> field "MinorImageVersion" Print.integer fields.UserMinor
    >> field "MajorSubsystemVersion" Print.integer fields.SubSysMajor
    >> field "MinorSubsystemVersion" Print.integer fields.SubSysMinor
    >> field "Win32VersionValue" Print.integer fields.Win32VersionValue
    >> field "SizeOfImage" Print.integer fields.ImageSize
    >> field "SizeOfHeaders" Print.integer fields.HeadersSize
    >> field "FileChecksum" Print.integer fields.FileChecksum
    >> field "Subsystem" Print.enumeration fields.Subsystem
    >> field "DllCharacteristics" Print.bitfield fields.DllFlags
    >> field "SizeOfStackReserve" Print.integer (uint32 fields.StackReserveSize) // NOTE: This is 8 bytes long instead for PE32+
    >> field "SizeOfStackCommit" Print.integer (uint32 fields.StackCommitSize) // NOTE: This is 8 bytes long instead for PE32+
    >> field "SizeOfHeapReserve" Print.integer (uint32 fields.HeapReserveSize) // NOTE: This is 8 bytes long instead for PE32+
    >> field "SizeOfHeapCommit" Print.integer (uint32 fields.HeapCommitSize) // NOTE: This is 8 bytes long instead for PE32+
    >> field "LoaderFlags" Print.integer fields.LoaderFlags
    >> field "NumberOfDataDirectories" Print.integer fields.NumberOfDataDirectories

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

let dataDirectories (directories: ParsedDataDirectories) offset =
    let rec inner i wr =
        match i with
        | -1 -> wr
        | _ ->
            let name =
                if i < dataDirectoryNames.Length
                then sprintf "%s Table" dataDirectoryNames.[i]
                else "Unknown"
            inner (i - 1) (field name Print.rvaAndSize directories.[i] wr)
    heading "Data Directories" offset >> inner (directories.Length - 1)

let sectionHeaders (headers: ParsedSectionHeaders) (offset: FileOffset) =
    // TODO: Use Seq.fold instead.
    let rec inner i wr =
        match i with
        | -1 -> wr
        | _ ->
            let header = headers.[i]
            heading (sprintf "\"%O\" Section Header" header.SectionName) (offset + (uint64 i * 40UL)) wr
            |> field "Name" (fun wr () -> wr.Write header.SectionName) () // TODO: Make sure length includes padding // TODO: Fix length
            |> field "VirtualSize" Print.integer header.Data.VirtualSize
            |> field "VirtualAddress" Print.integer header.Data.VirtualAddress
            |> field "SizeOfRawData" Print.integer header.Data.RawDataSize
            |> field "PointerToRawData" Print.integer header.Data.RawDataPointer
            |> field "PointerToRelocations" Print.integer header.PointerToRelocations
            |> field "PointerToLineNumbers" Print.integer header.PointerToLineNumbers
            |> field "NumberOfRelocations" Print.integer header.NumberOfRelocations
            |> field "NumberOfLineNumbers" Print.integer header.NumberOfLineNumbers
            |> field "Characteristics" Print.bitfield header.Characteristics
            |> ignore
            inner (i - 1) wr
    inner (headers.Length - 1)

let cliHeader (header: ParsedCliHeader) offset =
    heading "CLI Header" offset
    >> field "Cb" Print.integer header.Size
    >> field "MajorRuntimeVersion" Print.integer header.MajorRuntimeVersion
    >> field "MinorRuntimeVersion" Print.integer header.MinorRuntimeVersion
    >> field "MetaData" Print.rvaAndSize header.MetaData
    >> field "Flags" Print.bitfield header.Flags
    >> field "EntryPointToken" Print.integer header.EntryPointToken // TODO: Show what table the EntryPointToken refers to.
    >> field "Resources" Print.rvaAndSize header.Resources
    >> field "StrongNameSignature" Print.integer header.StrongNameSignature
    >> field "CodeManagerTable" Print.integer header.CodeManagerTable
    >> field "VTableFixups" Print.rvaAndSize header.VTableFixups
    >> field "ExportAddressTableJumps" Print.integer header.ExportAddressTableJumps
    >> field "ManagedNativeHeader" Print.integer header.ManagedNativeHeader

let metadataRoot (root: ParsedMetadataRoot) offset =
    heading "CLI Metadata Root" offset
    >> field "MajorVersion" Print.integer root.MajorVersion
    >> field "MinorVersion" Print.integer root.MinorVersion
    >> field "Reserved" Print.integer root.Reserved
    >> field "Length" Print.integer root.Version.Length
    // TODO: Make sure length includes padding
    >> fieldf "Version" root.Version.Length (fun wr () -> fprintf wr "\"%O\"" root.Version) ()
    >> field "Flags" Print.integer root.Flags
    >> field "Streams" Print.integer root.Streams

let streamHeader (header: ParsedStreamHeader) _ _ =
    field "Offset" Print.integer header.Offset
    >> field "Size" Print.integer header.Size
    >> fieldf
        "StreamName"
        header.Name.Length
        (fun wr () ->
            let name = header.Name.AsSpan()
            fprintf wr "\"%s\"" (Encoding.ASCII.GetString name))
        ()

let metadataTablesHeader header offset =
    let rec rows wr flags =
        match flags with
        | MetadataTableFlags.None -> ()
        | _ ->
            let flags' = flags >>> 1
            match header.Rows.TryGetValue(flags &&& MetadataTableFlags.Module) with
            | true, count -> invalidOp ""
            | false, _ -> ()
            rows wr flags'
    heading "Metadata Tables Header" offset
    >> field "Reserved" Print.integer header.Reserved1
    >> field "MajorVersion" Print.integer header.MajorVersion
    >> field "MinorVersion" Print.integer header.MinorVersion
    >> field "HeapSizes" Print.bitfield header.HeapSizes
    >> field "Reserved" Print.integer header.Reserved2
    >> field "Valid" Print.bitfield header.Valid
    >> field "Sorted" Print.bitfield header.Sorted
    >> fieldf "Rows" (4 * header.Rows.Count) rows header.Valid // TODO: Add brackets for list thing.

let handleError state error offset wr =
    eprintfn "error : %s" (ReadError.message state error offset)
    wr

let write headers =
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
       HandleError = handleError }
