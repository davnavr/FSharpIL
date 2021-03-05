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

let private testExec testf example (name: string) (dir: string) (path: string) fileName (test: Process -> _ -> unit): Test =
    fun() ->
        let file: FSharpIL.PortableExecutable.PEFile = example()
        let output = Path.Combine(dir, path)
        let executable = Path.Combine(output, fileName)
        let config = Path.Combine(output, "example.runtimeconfig.json")

        WritePE.toPath executable file

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
/// Builds a test case that executes the Portable Executable with the <c>dotnet exec</c> command.
/// </summary>
let testCaseExec = testExec testCase
let ftestCaseExec = testExec ftestCase
