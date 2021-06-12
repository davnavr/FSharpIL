namespace ILInfo

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
    let inline heading name (offset: FileOffset) out = comment out (fun wr -> fprintf wr "%s (%O)" name offset)
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
            newline out
            match header with
            | ParsedOptionalHeader.PE32(std, nt) ->
                heading "Optional Header (PE32)" offset out
                standardFieldsCommon std out
                field "BaseOfData" Print.integer std.BaseOfData out
                ntSpecificFields nt out
            | ParsedOptionalHeader.PE32Plus(std, nt) ->
                heading "Optional Header (PE32+)" offset out
                standardFieldsCommon std out
                ntSpecificFields nt out
            ValueSome out

    let write includeFileHeaders includeCilMetadata vfilter =
        let inline header printer =
            match includeFileHeaders with
            | IncludeHeaders -> ValueSome printer
            | NoHeaders -> ValueNone
        { PEFileReader.defaultReader with
            ReadLfanew = header (fun lfanew offset out ->
                heading "DOS Header" offset out
                field "lfanew" Print.integer (uint32 lfanew) out
                newline out
                ValueSome out)
            ReadCoffHeader = header Headers.coffHeader
            ReadOptionalHeader = header Headers.optionalHeader
            HandleError =
                fun state error offset out ->
                    eprintfn "error : %s" (ReadError.message state error offset)
                    out }
