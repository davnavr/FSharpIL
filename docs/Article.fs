namespace FSharpIL.Documentation

open System.IO

open FSharp.Formatting.CodeFormat
open FSharp.Formatting.Literate
open FSharp.Formatting.Literate.Evaluation
open FSharp.Formatting.Markdown

// TODO: Instead of having to allocate all of this stuff, maybe have a single function instead?
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type Article =
    { Document: LiterateDocument
      Title: string }

[<RequireQualifiedAccess>]
module internal Article =
    let private (|IsHeader|_|) size =
        function
        | Heading(size', [ Literal(text, _) ], _) when size' = size -> Some text
        | _ -> None

    let create evaluator (file: FileInfo) =
        let doc = Literate.ParseAndCheckScriptFile(file.FullName, fsiEvaluator = evaluator)
        { Document = doc
          Title = List.pick ((|IsHeader|_|) 1) doc.Paragraphs }

    let write article (output: Stream) =
        let content =
            main [] [
                !^(Literate.ToHtml(article.Document, generateAnchors = true))
            ]

        html output [ "lang", "us" ] [
            head [
                meta [ "charset", "utf-8" ]
                meta [ "name", "viewport"; "content", "width=device-width" ]
                title article.Title
            ]
            body [] [
                content
            ]
        ]
