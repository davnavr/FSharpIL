[<AutoOpen>]
module FSharpIL.ExampleHelpers

open Expecto

open System.Diagnostics
open System.IO

open Mono.Cecil

let private testCecil testf example (name: string) (test: _ -> unit): Test =
    fun() ->
        use metadata = example() |> WritePE.stream |> ModuleDefinition.ReadModule
        test metadata
    |> testf name

/// <summary>
/// Builds a test case that examines the Portable Executable with a <see cref="T:Mono.Cecil.ModuleDefinition"/>.
/// </summary>
let testCaseCecil = testCecil testCase
let ftestCaseCecil = testCecil ftestCase

let private writeToDisk example dir path fileName =
    let file: FSharpIL.PortableExecutable.PEFile = example()
    let output = Path.Combine(dir, path)
    let executable = Path.Combine(output, fileName)

    WritePE.toPath executable file

    output, executable

let private testExec testf example (name: string) dir path fileName (test: Process -> _ -> unit): Test =
    fun() ->
        let output, executable = writeToDisk example dir path fileName
        let config = Path.Combine(output, "example.runtimeconfig.json")

        use dotnet =
            ProcessStartInfo (
                fileName = "dotnet",
                arguments = sprintf "exec --runtimeconfig %s %s" config executable,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            )
            |> Process.Start

        let test' = test dotnet
        dotnet.WaitForExit()
        test'()
    |> testf name

/// <summary>
/// Builds a test case that writes the Portable Executable to disk and executes it with the <c>dotnet exec</c> command.
/// </summary>
let testCaseExec = testExec testCase
let ftestCaseExec = testExec ftestCase

let private testFile testf example (name: string) dir path fileName (test: _ -> unit): Test =
    testf name (fun() -> writeToDisk example dir path fileName |> test)

/// <summary>
/// Builds a test case that writes the Portable Executable file to disk.
/// </summary>
let testCaseFile = testFile testCase
let ftestCaseFile = testFile ftestCase

let private testLoad testf example (name: string) (test: _ -> unit): Test =
    fun() ->
        let context = new ExampleAssemblyLoadContext(name)
        try
            let file = example() |> WritePE.stream
            let assm = context.LoadFromStream file
            test assm
        finally context.Unload() // TODO: Figure out if unloading is necessary.
    |> testf name

/// <summary>
/// Builds a test case that loads the Portable Executable into a <see cref="T:System.Runtime.Loader.AssemblyLoadContext"/>.
/// </summary>
let testCaseLoad = testLoad testCase
let ftestCaseLoad = testLoad ftestCase
