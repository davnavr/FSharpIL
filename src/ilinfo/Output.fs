[<RequireQualifiedAccess>]
module ILInfo.Output

open System
open System.Collections.Generic

open FSharpIL.PortableExecutable
open FSharpIL.Reading

type Writer =
    { WriteHeader: uint64 -> string -> unit
      WriteField: string -> uint32 -> string -> unit
      WriteError: ErrorHandler<unit> }

type Kind =
    | Stdout
    //| Html

    interface Argu.IArgParserTemplate with
        member this.Usage =
            match this with
            | Stdout -> "the assembly information is directed to standard output"

let create =
    function
    | Stdout ->
        { WriteHeader = printfn "(0x%X) %s"
          WriteField = printfn " - %s (%i bytes) = %s"
          WriteError = fun state err offset wr -> stderr.WriteLine(ReadError.message state err offset); wr }

let private header (headers: ISet<FileHeader>) expected writer: Reader<_, _> =
    if headers.Contains expected
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
        let value =
            match directories.[i] with
            | struct(0u, 0u) -> "<zero>"
            | struct(rva, size) -> sprintf "(RVA = 0x%08X, Size = 0x%08X (%i))" rva size size
        wr.WriteField name 8u value
    wr

let write (headers: ISet<FileHeader>) =
    { MetadataReader.empty with
        ReadCoffHeader = header headers FileHeader.Coff coffHeader
        ReadStandardFields = header headers FileHeader.Standard standardFields
        ReadNTSpecificFields = header headers FileHeader.NT_Specific ntSpecificFields
        ReadDataDirectories = header headers FileHeader.Data_Directories dataDirectories
        HandleError = fun state error offset wr -> wr.WriteError state error offset (); wr }
