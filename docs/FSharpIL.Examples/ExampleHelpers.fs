[<AutoOpen>]
module FSharpIL.ExampleHelpers

open Expecto

open System.Diagnostics
open System.IO

open Mono.Cecil

[<RequireQualifiedAccess>]
module PEFile =
    let toCecilModule pe =
        WritePE.stream pe |> ModuleDefinition.ReadModule

let private testExec testf (Lazy file) (name: string) dir path fileName (test: Process -> _ -> unit): Test =
    fun() ->
        let output = Path.Combine(dir, path)
        let executable = Path.Combine(output, fileName)

        WritePE.toPath executable file

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
let testCaseExec file = testExec testCase file
let ftestCaseExec file = testExec ftestCase file
