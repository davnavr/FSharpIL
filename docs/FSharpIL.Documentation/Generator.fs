module FSharpIL.Documentation.Generator

open System.IO

open Argu

open FSharp.Formatting.Literate.Evaluation

exception private GenerationException of exn

type Arguments =
    | [<ExactlyOnce>] Content_Directory of content: string
    | [<ExactlyOnce>] Output_Directory of path: string
    | [<ExactlyOnce>] Style_Directory of style: string
    | [<Unique>] Launch_Debugger

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Content_Directory _ -> "specify the directory containing the documentation files"
            | Output_Directory _ -> "specify the directory where the resulting HTML documentation is written to"
            | Style_Directory _ -> "specify the directory containing CSS files"
            | Launch_Debugger -> "calls the Debugger.Launch method"

let private write (content: DirectoryInfo) (style: DirectoryInfo) (output: DirectoryInfo) =
    try
        if not output.Exists then output.Create()

        let evaluator = FsiEvaluator(strict = true)

        evaluator.EvaluationFailed.Add (failwithf "Exception thrown while evaluating expression: %A")

        let style' = output.CreateSubdirectory "style"
        for file in style.GetFiles() do
            file.CopyTo(Path.Combine(style'.FullName, file.Name)) |> ignore

        for script in content.GetFiles("*.fs", SearchOption.AllDirectories) do // TODO: Include .fsx files if necessary.
            let output =
                let name = Path.GetFileNameWithoutExtension script.FullName
                let path = Path.Combine(output.FullName, sprintf $"{name}.html")
                FileInfo path

            if output.Exists then
                failwithf "Duplicate documentation file %s generated from %s" output.FullName script.FullName

            output.Create() |> Article.write script evaluator
    with
    | ex -> raise (GenerationException ex)

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Arguments>()
    try
        let results = parser.ParseCommandLine(inputs = args)

        if results.Contains Launch_Debugger then
            System.Diagnostics.Debugger.Launch() |> ignore

        let content = results.GetResult Content_Directory |> DirectoryInfo
        let style = results.GetResult Style_Directory |> DirectoryInfo
        let output = results.GetResult Output_Directory |> DirectoryInfo
        write content style output
        0
    with
    | GenerationException e -> stderr.WriteLine e.Message; -1
    | ex ->
        match ex with
        | :? ArguParseException -> ()
        | _ -> stderr.WriteLine ex.Message

        parser.PrintUsage() |> stderr.WriteLine; -1
