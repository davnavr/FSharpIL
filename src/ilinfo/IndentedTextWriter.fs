namespace ILInfo

open System.IO

[<Sealed>]
type IndentedTextWriter (writer: TextWriter, level: int32) =
    inherit TextWriter()
    let indentation = System.String(' ', level)
    let mutable written = false
    member private _.WriteIndentation() =
        if not written then
            writer.Write indentation
            written <- true
    override this.Write(value: char) =
        this.WriteIndentation()
        writer.Write value
    override _.WriteLine() =
        writer.WriteLine()
        written <- false
    override _.Encoding = writer.Encoding
    override _.Close() = writer.Close()
