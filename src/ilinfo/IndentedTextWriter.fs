namespace ILInfo

open System.IO

[<Sealed>]
type IndentedTextWriter (writer: TextWriter, indentation: string) =
    inherit TextWriter()
    let mutable written, level = false, 0u
    member private _.WriteIndentation() =
        if not written then
            let mutable i = level
            while i > 0u do
                writer.Write indentation
                i <- i - 1u
            written <- true
    member _.Indent() = level <- level + 1u
    member _.Dedent() = if level > 0u then level <- level - 1u
    override this.Write(value: char) =
        this.WriteIndentation()
        writer.Write value
    override _.WriteLine() =
        writer.WriteLine()
        written <- false
    override _.Encoding = writer.Encoding
    override _.Close() = writer.Close()
