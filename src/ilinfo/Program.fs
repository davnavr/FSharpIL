module ILInfo.Program

open System
open System.IO

open Argu

open FSharpIL
open FSharpIL.Metadata

let [<Literal>] private description = "CLI metadata reader powered by FSharpIL (https://github.com/davnavr/FSharpIL)"

type Argument =
    | All
    | Coff_Header
    | Standard_Fields
    | NT_Specific_Fields
    | Data_Directories
    | Section_Headers
    | Cli_Header
    | Metadata_Root
    | Stream_Headers
    | Metadata_Tables_Header
    | Module_Table
    | [<ExactlyOnce>] File of string
    | [<Unique>] Launch_Debugger
    | [<Unique>] Output of Output.Kind

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | All -> "include all information that is read from the file in the output."
            | Coff_Header -> "include the values of the COFF header fields in the output."
            | Standard_Fields -> "includes the values of the optional header standard fields in the output."
            | NT_Specific_Fields -> "includes the values of the optional header NT-specific fields in the output."
            | Section_Headers -> "includes the values of the section headers' fields in the output."
            | Data_Directories -> "includes the RVAs and sizes of each data directory in the output."
            | Cli_Header -> "includes the contents of the CLI header in the output."
            | Metadata_Root -> "includes the contents of the CLI metadata root in the output."
            | Stream_Headers -> "includes the contents of each stream header in the output."
            | Metadata_Tables_Header -> "includes the values of the metadata header fields in the output."
            | Module_Table -> "includes the fields of the module table row in the output."
            | File _ -> "specifies the file containing the metadata to read."
            | Launch_Debugger -> "launches the debugger."
            | Output _ -> "specifies how the assembly information is outputted."

type ParsedArguments =
    { [<DefaultValue>] mutable File: string
      [<DefaultValue>] mutable IncludedHeaders: IncludedHeaders
      [<DefaultValue>] mutable IncludedTables: MetadataTableFlags
      [<DefaultValue>] mutable LaunchDebugger: bool
      mutable Output: Output.Kind }

    member this.AddHeader header = this.IncludedHeaders <- this.IncludedHeaders ||| header
    member this.AddTable table = this.IncludedTables <- this.IncludedTables ||| table

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
    let args' = { Output = Output.Stdout }

    for arg in result.GetAllResults() do
        match arg with
        | All ->
            args'.IncludedHeaders <- IncludedHeaders.All
            args'.IncludedTables <- LanguagePrimitives.EnumOfValue(UInt64.MaxValue)
        | Coff_Header -> args'.AddHeader IncludedHeaders.CoffHeader
        | Standard_Fields -> args'.AddHeader IncludedHeaders.StandardFields
        | NT_Specific_Fields -> args'.AddHeader IncludedHeaders.NTSpecificFields
        | Section_Headers -> args'.AddHeader IncludedHeaders.SectionHeaders
        | Data_Directories -> args'.AddHeader IncludedHeaders.DataDirectories
        | Cli_Header -> args'.AddHeader IncludedHeaders.CliHeader
        | Metadata_Root -> args'.AddHeader IncludedHeaders.MetadataRoot
        | Stream_Headers -> args'.AddHeader IncludedHeaders.StreamHeaders
        | Metadata_Tables_Header -> args'.AddHeader IncludedHeaders.MetadataTables
        | Module_Table -> args'.AddTable MetadataTableFlags.Module
        | File file -> args'.File <- file
        | Launch_Debugger -> args'.LaunchDebugger <- true
        | Output output -> args'.Output <- output

    if args'.LaunchDebugger then System.Diagnostics.Debugger.Launch() |> ignore

    match args' with
    | { File = ValidFile file } ->
        use reader = file.OpenRead()
        ReadCli.fromStream
            reader
            (Output.create args'.Output)
            (Output.write args'.IncludedHeaders args'.IncludedTables)
            |> ignore
        0
    | { File = NotFound path } -> exitfn "The file \"%s\" does not exist." path
    | { File = InvalidPath path } -> exitfn "The file \"%s\" is invalid." path
    | { File = UnauthorizedAccess path } -> exitfn "Cannot access the file \"%s\"." path
