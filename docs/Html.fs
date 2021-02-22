[<AutoOpen>]
module internal FSharpIL.Documentation.Html

open System.IO

type Element = StreamWriter -> unit

let tag (name: string) attributes (content: Element[]) (writer: StreamWriter) =
    writer.Write '<'
    writer.Write name
    for (attr: string, value: string) in attributes do
        writer.Write ' '
        writer.Write attr
        writer.Write "=\""
        writer.Write value
        writer.Write '"'
    match content with
    | [||] -> writer.Write "/>"
    | _ ->
        writer.Write '>'
        for elem in content do elem writer
        writer.Write "</"
        writer.Write name
        writer.Write '>'

let html (stream: Stream) attributes content =
    use writer = new StreamWriter(stream)
    writer.WriteLine("<!DOCTYPE html>")
    tag "html" attributes content writer

let head attributes content = tag "head" attributes content
let body attributes content = tag "body" attributes content
