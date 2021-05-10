module ILInfo.Program

open System
open System.IO

open Argu

type Argument =
    | Headers
    | Heaps
    | No_IL
    | Public_Only
    | Visibility of VisibilityFilter list
    | [<ExactlyOnce>] File of path: string
    | [<Unique>] Format of Output.Format
    | Launch_Debugger
    | [<AltCommandLine "-o"; Unique>] Output of path: string

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Headers -> "Include fields of file headers in the output."
            | Heaps -> "Include the raw metadata heaps in the output."
            | No_IL -> "Exclude IL output."
            | Public_Only -> "Only include items with public visibility in the output."
            | Visibility _ -> "Only include items with the specified visibility."
            | File _ -> "Read input from the specified file."
            | Format _ -> "Output in the specified format."
            | Launch_Debugger -> "Launch the debugger."
            | Output _ -> "Direct output to the specified file instead of to standard output."

type ParsedArguments =
    { [<DefaultValue>] mutable IncludeHeaders: IncludeHeaders
      [<DefaultValue>] mutable IncludeIL: IncludeIL
      [<DefaultValue>] mutable InputFile: string
      [<DefaultValue>] mutable LaunchDebugger: bool
      mutable Format: Output.Format
      mutable OutputKind: OutputKind
      mutable VisibilityFilter: VisibilityFilter }

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

let exitfn format = Printf.kprintf (fun msg -> eprintfn "error : %s" msg; -1) format

// TODO: Rename to fsdasm
[<EntryPoint>]
let main args =
    let parser = ArgumentParser.Create<Argument>()
    try
        let result = parser.ParseCommandLine args
        let args' = { Format = Output.IL; OutputKind = OutputKind.Console; VisibilityFilter = VisibilityFilter.Public }

        for arg in result.GetAllResults() do
            match arg with
            | Headers -> args'.IncludeHeaders <- IncludeHeaders
            | Heaps -> failwith "TODO: Allow printing of raw metadata heaps"
            | No_IL -> args'.IncludeIL <- NoIL
            | Public_Only -> args'.VisibilityFilter <- VisibilityFilter.Public
            | Visibility vis -> args'.VisibilityFilter <- List.reduce (|||) vis
            | File file -> args'.InputFile <- file
            | Format format -> args'.Format <- format
            | Launch_Debugger -> args'.LaunchDebugger <- true
            | Output file -> args'.OutputKind <- OutputKind.File file

        if args'.LaunchDebugger then System.Diagnostics.Debugger.Launch() |> ignore

        match args' with
        | { InputFile = NotFound path }
        | { OutputKind = OutputKind.File(NotFound path) } -> exitfn "The file \"%s\" does not exist." path
        | { InputFile = InvalidPath path }
        | { OutputKind = OutputKind.File(InvalidPath path) } -> exitfn "The file \"%s\" is invalid." path
        | { InputFile = UnauthorizedAccess path }
        | { OutputKind = OutputKind.File(UnauthorizedAccess path) } -> exitfn "Cannot access the file \"%s\"." path
        | { InputFile = ValidFile file; OutputKind = output } ->
            use reader = file.OpenRead()
            use output' =
                match output with
                | OutputKind.Console -> stdout
                | OutputKind.File path -> new StreamWriter(path) :> TextWriter
            Output.write args'.Format args'.IncludeHeaders args'.IncludeIL args'.VisibilityFilter
            |> FSharpIL.ReadCli.fromStream reader output'
            |> ignore
            0
    with
    | :? ArguException as e ->
        stderr.WriteLine e.Message
        -1
