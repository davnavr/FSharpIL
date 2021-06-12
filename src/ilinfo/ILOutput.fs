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
    let inline fieldf name size printer value out = comment out (fun wr -> fprintf wr "// %s (%i bytes) = %a" name size printer value)
    let inline field name printer (value: 'Value) out = fieldf name sizeof<'Value> printer value out

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
                ValueSome out)
            ReadCoffHeader = header Headers.coffHeader
            HandleError =
                fun state error offset out ->
                    eprintfn "error : %s" (ReadError.message state error offset)
                    out }
