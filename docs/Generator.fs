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

        let fsi = FsiEvaluator(strict = true)
        let evaluator =
            let fsi' = fsi :> IFsiEvaluator
            { new IFsiEvaluator with
                member _.Evaluate(code, asExpression, file) =
                    printfn "Evaluating: %s" code

                    let result = fsi'.Evaluate(code, asExpression, file)

                    printfn "Result: %A" result
#if DEBUG
                    // Workaround to stop generation of documentation if the script is invalid.
                    match result :?> FsiEvaluationResult with
                    | { FsiOutput = Some output } when output.Contains "file may be locked" || output.Contains "Binding session to" -> ()
                    | { Output = Some ""; FsiOutput = Some error; ItValue = None; Result = None } ->
                        let file' =
                            Option.map (sprintf "while evaluating file %s") file |> Option.defaultValue ""
                        failwithf "Possible evaluation error %s: %s" file' error
                    | { Output = Some "" }
                    | { Output = None }
                    | { FsiOutput = Some "" }
                    | { FsiOutput = None } as result ->
                        failwithf "Empty output for file %A, result is %A" file result
                    | _ -> ()
#endif
                    result
                member _.Format(result, kind, executionCount) =
                    fsi'.Format(result, kind, executionCount) }

        fsi.EvaluationFailed.Add (failwithf "Exception thrown while evaluating expression: %A")

        let style' = output.CreateSubdirectory "style"
        for file in style.GetFiles() do
            file.CopyTo(Path.Combine(style'.FullName, file.Name)) |> ignore

        for script in content.GetFiles("*.fsx", SearchOption.AllDirectories) do
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
