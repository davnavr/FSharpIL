#if FAKE_DEPENDENCIES
#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
//"
#endif

#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO.FileSystemOperators

module DotNetCli = Fake.DotNet.DotNet

let rootDir = __SOURCE_DIRECTORY__
let testDir = rootDir </> "test"

let slnFile = rootDir </> "FSharpIL.sln"

let handleErr msg: ProcessResult -> _ =
    function
    | { ExitCode = ecode } when ecode <> 0 ->
        failwithf "Process exited with code %i: %s" ecode msg
    | _ -> ()

let runProj args proj =
    sprintf
        "--project %s --no-restore --configuration Release -- %s"
        proj
        args
    |> DotNetCli.exec id "run"

Target.create "Clean" (fun _ ->
    slnFile
    |> DotNetCli.exec id "clean"
    |> ignore
)

Target.create "Build" (fun _ ->
    DotNetCli.build
        (fun opt ->
            { opt with
                Configuration = DotNetCli.Release
                NoRestore = true })
        slnFile
)

Target.create "Test" (fun _ ->
    testDir </> "FSharpIL.Tests" </> "FSharpIL.Tests.fsproj"
    |> runProj ""
    |> handleErr "One or more tests failed"
)

Target.create "Publish" (fun _ ->
    Trace.trace "Publishing..."
)

"Clean"
==> "Build"
==> "Test"
==> "Publish"

Target.runOrDefault "Publish"
