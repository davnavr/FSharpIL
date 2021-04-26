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

let coffHeader (header: ParsedCoffHeader) offset (wr: Writer) =
    wr.WriteHeader offset "COFF Header"
    writeEnum "Machine" header.Machine wr
    writeInt "NumberOfSections" header.NumberOfSections wr
    writeInt "TimeDateStamp" header.TimeDateStamp wr
    writeInt "SymbolTablePointer" header.SymbolTablePointer wr
    writeInt "SymbolCount" header.SymbolCount wr
    writeInt "OptionalHeaderSize" header.OptionalHeaderSize wr
    writeEnum "Characteristics" header.Characteristics wr
    wr

let standardFields (fields: ParsedStandardFields) offset (wr: Writer) =
    wr.WriteHeader offset "Standard Fields"
    writeEnum "Magic" fields.Magic wr
    writeInt "MajorLinkerVersion" fields.LMajor wr
    writeInt "MinorLinkerVersion" fields.LMinor wr
    writeInt "CodeSize" fields.CodeSize wr
    writeInt "InitializedDataSize" fields.InitializedDataSize wr
    writeInt "UninitializedDataSize" fields.UninitializedDataSize wr
    writeInt "EntryPointRva" fields.EntryPointRva wr
    writeInt "BaseOfCode" fields.BaseOfCode wr
    ValueOption.iter (fun bdata -> writeInt "BaseOfData" bdata wr) fields.BaseOfData
    wr

let write (headers: ISet<FileHeader>) =
    { MetadataReader.empty with
        ReadCoffHeader = header headers FileHeader.Coff coffHeader
        ReadStandardFields = header headers FileHeader.Standard standardFields
        HandleError = fun state error offset wr -> wr.WriteError state error offset (); wr }
