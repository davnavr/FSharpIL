module FSharpIL.Documentation.Generator

open System.IO

open Argu

open FSharp.Formatting.CodeFormat
open FSharp.Formatting.Literate
open FSharp.Formatting.Literate.Evaluation
open FSharp.Formatting.Markdown

exception private GenerationException of exn

type Arguments =
    | [<ExactlyOnce>] Content_Directory of content: string
    | [<ExactlyOnce>] Output_Directory of path: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Content_Directory _ -> "specify the directory containing the documentation files"
            | Output_Directory _ -> "specify the directory where the resulting HTML documentation is written to"

let private write (content: DirectoryInfo) (output: DirectoryInfo) =
    try
        let evaluator = FsiEvaluator()

        for script in content.GetFiles("*.fsx", SearchOption.AllDirectories) do
            Article.write script
            ()
    with
    | ex -> raise (GenerationException ex)

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Arguments>()
    try
        let results = parser.ParseCommandLine(inputs = args)
        let content = results.GetResult Content_Directory |> DirectoryInfo
        let output = results.GetResult Output_Directory |> DirectoryInfo
        write content output
        0
    with
    | GenerationException e -> stderr.WriteLine e.Message; -1
    | ex ->
        match ex with
        | :? ArguParseException -> ()
        | _ -> stderr.WriteLine ex.Message

        parser.PrintUsage() |> stderr.WriteLine; -1
