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
open Fake.IO.Globbing.Operators

let rootDir = __SOURCE_DIRECTORY__
let docsDir = rootDir </> "docs"
let docsContentDir = docsDir </> "content"
let outDir = rootDir </> "out"
let testDir = rootDir </> "test"

let slnFile = rootDir </> "FSharpIL.sln"

let handleErr msg: ProcessResult -> _ =
    function
    | { ExitCode = ecode } when ecode <> 0 ->
        failwithf "Process exited with code %i: %s" ecode msg
    | _ -> ()

Target.create "Clean" <| fun _ ->
    List.iter Shell.cleanDir [ outDir; docsDir </> "out" ]

    DotNet.exec id "clean" slnFile |> handleErr "Error occured while cleaning project output"

Target.create "Build" <| fun _ ->
    DotNet.build
        (fun opt ->
            { opt with
                Configuration = DotNet.Release
                NoRestore = true })
        slnFile

Target.create "Test Examples" <| fun _ ->
    !!(docsContentDir </> "**/*.fsx")
    |> Seq.map (sprintf "--use:%s")
    |> String.concat " "
    |> sprintf "--exec --quiet --nologo --targetprofile:netcore %s"
    |> DotNet.exec id "fsi"
    |> handleErr "One or more examples are invalid"

Target.create "Build Documentation" <| fun _ ->
    sprintf
        "-p %s -c Release --no-build --no-restore -- --content-directory %s --style-directory %s --output-directory %s"
        (docsDir </> "FSharpIL.Documentation.fsproj")
        docsContentDir
        (docsDir </> "style")
        (outDir </> "docs")
    |> DotNet.exec id "run"
    |> handleErr "Error occured while generating documentation"

// TODO: Since exceptions may not be handled well in documentation .fsx files, consider running them by calling the F# interactive here.
// Target.create "Test Examples"

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

"Build"
==> "Test Examples"
==> "Build Documentation"
==> "Publish"

Target.runOrDefault "Publish"
