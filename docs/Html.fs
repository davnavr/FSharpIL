[<AutoOpen>]
module internal FSharpIL.Documentation.Html

open System.IO

type Element = StreamWriter -> unit

let inline (!^) (text: string) =
    fun (writer: StreamWriter) -> writer.Write text

let tag (name: string) attributes content (writer: StreamWriter) =
    writer.Write '<'
    writer.Write name
    for (attr: string, value: string) in attributes do
        writer.Write ' '
        writer.Write attr
        writer.Write "=\""
        writer.Write value
        writer.Write '"'
    if Seq.isEmpty content
    then writer.Write "/>"
    else
        writer.Write '>'
        for (elem: Element) in content do elem writer
        writer.Write "</"
        writer.Write name
        writer.Write '>'

let html (stream: Stream) attributes content =
    use writer = new StreamWriter(stream)
    writer.WriteLine("<!DOCTYPE html>")
    tag "html" attributes content writer

let head content: Element = tag "head" Seq.empty content
let meta attributes: Element = tag "meta" attributes Seq.empty
let title name: Element = tag "title" Seq.empty [ !^name ]

let body attributes content: Element = tag "body" attributes content
let main attributes content: Element = tag "main" attributes content
