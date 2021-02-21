#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem //"
#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators

let rootDir = __SOURCE_DIRECTORY__
let outDir = rootDir </> "out"
let testDir = rootDir </> "test"

let slnFile = rootDir </> "FSharpIL.sln"

let handleErr msg: ProcessResult -> _ =
    function
    | { ExitCode = ecode } when ecode <> 0 ->
        failwithf "Process exited with code %i: %s" ecode msg
    | _ -> ()

Target.create "Clean" <| fun _ ->
    Shell.cleanDir outDir
    DotNet.exec id "clean" slnFile |> handleErr "Error occured while cleaning project output"

Target.create "Build" <| fun _ ->
    DotNet.build
        (fun opt ->
            { opt with
                Configuration = DotNet.Release
                NoRestore = true })
        slnFile

Target.create "Build Documentation" <| fun _ ->
    rootDir </> "docs" </> "build-docs.fsx"
    |> sprintf "--exec --nologo --optimize --targetprofile:netcore %s"
    |> DotNet.exec id "fsi"
    |> handleErr "Error occured while generating documentation"

Target.create "Test" <| fun _ ->
    let proj = testDir </> "FSharpIL.Tests" </> "FSharpIL.Tests.fsproj"
    for tfm in [ "netcoreapp3.1"; "net5.0" ] do
        sprintf
            "-p %s -c Release -f %s --no-build --no-restore"
            proj
            tfm
        |> DotNet.exec id "run"
        |> handleErr "One or more tests failed"

Target.create "Publish" <| fun _ ->
    Trace.trace "Publishing..."

"Clean"
==> "Build"
==> "Test"
==> "Publish"

"Build" ==> "Build Documentation" ==> "Publish"

Target.runOrDefault "Publish"
