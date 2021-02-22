namespace FSharpIL.Documentation

open System.Collections.Generic
open System.IO

open FSharp.Formatting.CodeFormat
open FSharp.Formatting.Literate
open FSharp.Formatting.Literate.Evaluation
open FSharp.Formatting.Markdown

type Article =
    { mutable Title: string
      Sections: List<string> }

[<RequireQualifiedAccess>]
module internal Article =
    let inline private (|Header|_|) size =
        function
        | Heading(size', [ Literal(text, _) ], _) when size' = size -> Some text
        | _ -> None

    let write (article: FileInfo) evaluator (output: Stream) =
        let doc = Literate.ParseAndCheckScriptFile(article.FullName, fsiEvaluator = evaluator)
        let info =
            { Title = null
              Sections = List<_>() }

        for paragraph in doc.Paragraphs do
            match paragraph with
            | Header 1 title when info.Title = null -> info.Title <- title
            | Header 2 section -> info.Sections.Add section
            | _ -> ()

        let content =
            main [] [
                fun writer -> Literate.WriteHtml(doc, writer, lineNumbers = true, generateAnchors = true)
            ]

        html output [ "lang", "us" ] [
            head [
                meta [ "charset", "utf-8" ]
                meta [ "name", "viewport"; "content", "width=device-width" ]
                title info.Title
            ]
            body [] [
                content
            ]
        ]
