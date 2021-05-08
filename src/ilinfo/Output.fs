[<RequireQualifiedAccess>]
module ILInfo.Output

open System
open System.Runtime.CompilerServices
open System.Text

open FSharpIL.Metadata
open FSharpIL.PortableExecutable
open FSharpIL.Reading

type Writer =
    { WriteHeader: FileOffset -> string -> unit
      WriteField: string -> uint32 -> string -> unit
      WriteError: ErrorHandler<unit> }

[<IsReadOnly; Struct>]
type Format =
    | IL
    | Html

let create =
    function
    | IL ->
        { WriteHeader = fun offset -> printfn "(0x%X) %s" (uint64 offset)
          WriteField = printfn " - %s (%i bytes) = %s"
          WriteError = fun state err offset wr -> stderr.WriteLine(ReadError.message state err offset); wr }
    | Html -> invalidOp "HTML output is not yet supported"

let private header (flags: 'Flags when 'Flags :> Enum) (expected: 'Flags) writer: Reader<_, _> =
    if flags.HasFlag expected
    then ValueSome writer
    else ValueNone

let inline private zeroes size (digits: string) = String('0', (2 * size) - digits.Length)

let inline private writeInt name (value: 'Integer) { WriteField = write } =
    let size = sizeof<'Integer>
    let value' = sprintf "%X" value
    write name (uint32 size) (sprintf "0x%s%s (%i)" (zeroes size value') value' value)

let inline private writeEnum name (value: 'Enum when 'Enum :> Enum) { WriteField = write } =
    let size = sizeof<'Enum>
    let value' = value.ToString "X"
    write name (uint32 size) (sprintf "0x%s%s (%O)" (zeroes size value') value' value)

let inline private writeString name (value: byte[]) { WriteField = write } =
    Seq.map (sprintf "%02X") value
    |> String.concat " "
    |> sprintf "\"%s\" [%s]" (Encoding.ASCII.GetString value)
    |> write name (uint32 value.Length)

let inline private writeRvaAndSize name rvaAndSize wr =
    match rvaAndSize with
    | { Rva = 0u; Size = 0u } -> "<zero>"
    | { Rva = rva; Size = size } -> sprintf "(RVA = 0x%08X, Size = 0x%08X (%i))" rva size size
    |> wr.WriteField name 8u

let coffHeader (header: ParsedCoffHeader) offset wr =
    wr.WriteHeader offset "COFF Header"
    writeEnum "Machine" header.Machine wr
    writeInt "NumberOfSections" header.NumberOfSections wr
    writeInt "TimeDateStamp" header.TimeDateStamp wr
    writeInt "SymbolTablePointer" header.SymbolTablePointer wr
    writeInt "SymbolCount" header.SymbolCount wr
    writeInt "OptionalHeaderSize" header.OptionalHeaderSize wr
    writeEnum "Characteristics" header.Characteristics wr
    wr

let standardFields (fields: ParsedStandardFields) offset wr =
    wr.WriteHeader offset "Standard Fields"
    writeEnum "Magic" fields.Magic wr
    writeInt "MajorLinkerVersion" fields.LMajor wr
    writeInt "MinorLinkerVersion" fields.LMinor wr
    writeInt "SizeOfCode" fields.CodeSize wr
    writeInt "SizeOfInitializedData" fields.InitializedDataSize wr
    writeInt "SizeOfUninitializedData" fields.UninitializedDataSize wr
    writeInt "EntryPointRva" fields.EntryPointRva wr
    writeInt "BaseOfCode" fields.BaseOfCode wr
    ValueOption.iter (fun bdata -> writeInt "BaseOfData" bdata wr) fields.BaseOfData
    wr

let ntSpecificFields (fields: ParsedNTSpecificFields) offset wr =
    wr.WriteHeader offset "NT Specific Fields"
    // TODO: Write different size fields if PE32+
    //ImageBase
    let (salignment, falignment) = fields.Alignment
    writeInt "SectionAlignment" salignment wr
    writeInt "FileAlignment" falignment wr
    writeInt "MajorOperatingSystemVersion" fields.OSMajor wr
    writeInt "MinorOperatingSystemVersion" fields.OSMinor wr
    writeInt "MajorImageVersion" fields.UserMajor wr
    writeInt "MinorImageVersion" fields.UserMinor wr
    writeInt "MajorSubsystemVersion" fields.SubSysMajor wr
    writeInt "MinorSubsystemVersion" fields.SubSysMinor wr
    writeInt "Win32VersionValue" fields.Win32VersionValue wr
    writeInt "SizeOfImage" fields.ImageSize wr
    writeInt "SizeOfHeaders" fields.HeadersSize wr
    writeInt "FileChecksum" fields.FileChecksum wr
    writeEnum "Subsystem" fields.Subsystem wr
    writeEnum "DllCharacteristics" fields.DllFlags wr
    //StackReserveSize
    writeInt "LoaderFlags" fields.LoaderFlags wr
    writeInt "NumberOfDataDirectories" fields.NumberOfDataDirectories wr
    wr

let dataDirectories (directories: ParsedDataDirectories) offset wr =
    wr.WriteHeader offset "Data Directories"
    let names =
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
    for i = 0 to directories.Length - 1 do
        let name =
            if i < names.Length
            then names.[i]
            else "Unknown"
        writeRvaAndSize name directories.[i] wr
    wr

let sectionHeaders (headers: ParsedSectionHeaders) (offset: FileOffset) wr =
    for i = 0 to headers.Length - 1 do
        let header = headers.[i]
        wr.WriteHeader (offset + (uint64 i * 40UL)) (sprintf "\"%O\" Section Header" header.SectionName)
        writeString "Name" (SectionName.toArray header.SectionName) wr // TODO: Make sure length includes padding
        writeInt "VirtualSize" header.Data.VirtualSize wr
        writeInt "VirtualAddress" header.Data.VirtualAddress wr
        writeInt "SizeOfRawData" header.Data.RawDataSize wr
        writeInt "PointerToRawData" header.Data.RawDataPointer wr
        writeInt "PointerToRelocations" header.PointerToRelocations wr
        // PointerToLineNumbers
        writeInt "NumberOfRelocations" header.NumberOfRelocations wr
        // NumberOfLineNumbers
        writeEnum "Characteristics" header.Characteristics wr
    wr

let cliHeader (header: ParsedCliHeader) offset wr =
    wr.WriteHeader offset "CLI Header"
    writeInt "Cb" header.Size wr
    writeInt "MajorRuntimeVersion" header.MajorRuntimeVersion wr
    writeInt "MinorRuntimeVersion" header.MinorRuntimeVersion wr
    writeRvaAndSize "MetaData" header.MetaData wr
    writeEnum "Flags" header.Flags wr
    writeInt "EntryPointToken" header.EntryPointToken wr // TODO: Show what table the EntryPointToken refers to.
    writeRvaAndSize "Resources" header.Resources wr
    writeInt "StrongNameSignature" header.StrongNameSignature wr
    writeInt "CodeManagerTable" header.CodeManagerTable wr
    writeRvaAndSize "VTableFixups" header.VTableFixups wr
    writeInt "ExportAddressTableJumps" header.ExportAddressTableJumps wr
    writeInt "ManagedNativeHeader" header.ManagedNativeHeader wr
    wr

let metadataRoot (root: ParsedMetadataRoot) offset wr =
    wr.WriteHeader offset "CLI Metadata Root"
    writeInt "MajorVersion" root.MajorVersion wr
    writeInt "MinorVersion" root.MinorVersion wr
    writeInt "Reserved" root.Reserved wr
    writeInt "Length" root.Version.Length wr
    writeString "Version" (MetadataVersion.toArray root.Version) wr // TODO: Make sure length includes padding // TODO: Should printed version string use UTF-8?
    writeInt "Flags" root.Flags wr
    writeInt "Streams" root.Streams wr
    wr

let streamHeader (header: ParsedStreamHeader) (_: int32) offset wr =
    let mutable name = header.Name

    Encoding.ASCII.GetString(name.AsSpan())
    |> sprintf "\"%s\" Stream Header"
    |> wr.WriteHeader offset

    writeInt "Offset" header.Offset wr
    writeInt "Size" header.Size wr
    writeString "Name" (Unsafe.As &name) wr
    wr

let metadataTablesHeader (header: ParsedMetadataTablesHeader) offset wr =
    wr.WriteHeader offset "Metadata Tables Header"
    writeInt "Reserved" header.Reserved1 wr
    writeInt "MajorVersion" header.MajorVersion wr
    writeInt "MinorVersion" header.MinorVersion wr
    writeEnum "HeapSizes" header.HeapSizes wr
    writeInt "Reserved" header.Reserved2 wr
    writeEnum "Valid" header.Valid wr
    writeEnum "Sorted" header.Sorted wr
    
    Seq.map (fun (KeyValue(_, i)) -> sprintf "0x%08X" i) header.Rows
    |> String.concat ", "
    |> sprintf "[%s]"
    |> wr.WriteField "Rows" (4u * uint32 header.Rows.Count)

    wr

let metadataTables (tables: ParsedMetadataTables) offset wr =
    metadataTablesHeader tables.Header offset wr |> ignore // TODO: Fix, offset is for ParsedMetadataTables, not for header.

    for i = 0 to int32 tables.Module.RowCount - 1 do
        printfn ".module "

    wr

let write hflags tflags =
    { MetadataReader.empty with
       ReadCoffHeader = header hflags IncludedHeaders.CoffHeader coffHeader
       ReadStandardFields = header hflags IncludedHeaders.StandardFields standardFields
       ReadNTSpecificFields = header hflags IncludedHeaders.NTSpecificFields ntSpecificFields
       ReadDataDirectories = header hflags IncludedHeaders.DataDirectories dataDirectories
       ReadSectionHeaders = header hflags IncludedHeaders.SectionHeaders sectionHeaders
       ReadCliHeader = header hflags IncludedHeaders.CliHeader cliHeader
       ReadMetadataRoot = header hflags IncludedHeaders.MetadataRoot metadataRoot
       ReadStreamHeader =
           if hflags.HasFlag IncludedHeaders.StreamHeaders
           then ValueSome streamHeader
           else ValueNone
       ReadMetadataTables = ValueSome metadataTables
       HandleError = fun state error offset wr -> wr.WriteError state error offset (); wr }
