module ILInfo.Program

open System
open System.Collections.Generic
open System.IO

open Argu

open FSharpIL

let [<Literal>] private description = "CLI metadata reader powered by FSharpIL (https://github.com/davnavr/FSharpIL)"

type Argument =
    | [<ExactlyOnce>] File of string
    | File_Header of FileHeader
    | [<Unique>] Launch_Debugger
    | [<Unique>] Output of Output.Kind

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | File_Header _ -> "include the values of a PE file header's fields in the output."
            | File _ -> "specifies the file containing the metadata to read."
            | Launch_Debugger -> "launches the debugger."
            | Output _ -> "specifies how the assembly information is outputted."

type ParsedArguments =
    { [<DefaultValue>] mutable File: string
      [<DefaultValue>] mutable LaunchDebugger: bool
      FileHeaders: HashSet<FileHeader>
      mutable Output: Output.Kind }

let (|ValidFile|NotFound|InvalidPath|UnauthorizedAccess|) path =
    try
        let file = FileInfo path
        if file.Exists
        then ValidFile file
        else NotFound path
    with
    | :? ArgumentException
    | :? PathTooLongException -> InvalidPath path
    | :? UnauthorizedAccessException -> UnauthorizedAccess path

let exitfn format = Printf.kprintf (fun msg -> eprintfn "%s" msg; -1) format

[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Argument>(programName = "ilinfo", helpTextMessage = description)
    let result = parser.ParseCommandLine(args, raiseOnUsage = false)
    let args' = { Output = Output.Stdout; FileHeaders = HashSet 5 }

    for arg in result.GetAllResults() do
        match arg with
        | File file -> args'.File <- file
        | File_Header header -> args'.FileHeaders.Add header |> ignore
        | Launch_Debugger -> args'.LaunchDebugger <- true
        | Output output -> args'.Output <- output

    if args'.LaunchDebugger then System.Diagnostics.Debugger.Launch() |> ignore

    match args' with
    | { File = ValidFile file } ->
        use reader = file.OpenRead()
        ReadCli.fromStream reader (Output.create args'.Output) (Output.write args'.FileHeaders) |> ignore
        0
    | { File = NotFound path } -> exitfn "The file \"%s\" does not exist." path
    | { File = InvalidPath path } -> exitfn "The file \"%s\" is invalid." path
    | { File = UnauthorizedAccess path } -> exitfn "Cannot access the file \"%s\"." path
