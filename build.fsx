#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem //"
#load "./.fake/build.fsx/intellisense.fsx"

open System.IO

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators

let rootDir = __SOURCE_DIRECTORY__
let docsDir = rootDir </> "docs"
let outDir = rootDir </> "out"
let testDir = rootDir </> "test"

let slnFile = rootDir </> "FSharpIL.sln"

let handleErr msg: ProcessResult -> _ =
    function
    | { ExitCode = ecode } when ecode <> 0 ->
        failwithf "Process exited with code %i: %s" ecode msg
    | { ExitCode = 0; Results = messages } ->
        for { IsError = err; Message = msg } in messages do
            if err || msg.Contains "Test failed" then failwith msg
    | _ -> ()

Target.create "Clean" <| fun _ ->
    [
        // All example outputs
        !!(docsDir </> "content" </> "exout" </> "*.dll")

        // All outputs from previous build
        !!(outDir </> "**" </> "*") -- (outDir </> ".gitignore")
    ]
    |> List.iter File.deleteAll

    !!(rootDir </> "**" </> "*.fsproj")
    |> Seq.collect
        (fun proj ->
            let proj' = FileInfo proj
            proj'.Directory.GetDirectories())
    |> Seq.iter
        (fun dir ->
            match dir.Name with
            | "bin"
            | "obj" -> dir.Delete true
            | _ -> ())

Target.create "Restore" <| fun _ ->
    DotNet.restore id slnFile

Target.create "BuildAll" <| fun _ ->
    DotNet.build
        (fun opt ->
            { opt with
                Configuration = DotNet.Release
                NoRestore = true })
        slnFile

Target.create "GenerateExamples" <| fun _ ->
    docsDir </> "FSharpIL.Examples" </> "FSharpIL.Examples.fsproj"
    |> sprintf "-p %s -c Release --no-build --no-restore"
    |> DotNet.exec id "run"
    |> handleErr "One or more examples is invalid"

Target.create "BuildDocumentation" <| fun _ ->
    sprintf
        "-p %s -c Release --no-build --no-restore -- --content-directory %s --style-directory %s --output-directory %s"
        (docsDir </> "FSharpIL.Documentation" </> "FSharpIL.Documentation.fsproj")
        (docsDir </> "content")
        (docsDir </> "style")
        (outDir </> "docs")
    |> DotNet.exec id "run"
    |> handleErr "Error occured while generating documentation"

Target.create "CheckProperties" <| fun _ ->
    let proj = testDir </> "FSharpIL.Properties" </> "FSharpIL.Properties.fsproj"
    for tfm in [ "netcoreapp3.1"; "net5.0" ] do
        sprintf
            "-p %s -c Release -f %s --no-build --no-restore"
            proj
            tfm
        |> DotNet.exec id "run"
        |> handleErr "One or more tests failed"

Target.create "TestExamples" <| fun _ ->
    let proj = testDir </> "FSharpIL.Examples.Tests" </> "FSharpIL.Examples.Tests.fsproj"

    DotNet.build
        (fun opt -> { opt with Configuration = DotNet.Release })
        proj

    sprintf "-p %s -c Release --no-build --no-restore" proj
    |> DotNet.exec id "run"
    |> handleErr "One or more tests failed"

Target.create "Pack" <| fun _ ->
    Trace.trace "Packing..."

"Clean"
==> "Restore"
==> "BuildAll"
==> "CheckProperties"
==> "Pack"

"BuildAll"
==> "GenerateExamples"
==> "TestExamples"
==> "BuildDocumentation"
==> "Pack"

Target.runOrDefault "Pack"
