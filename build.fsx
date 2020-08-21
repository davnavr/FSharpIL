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

Target.create "Clean" (fun _ ->
    slnFile
    |> DotNetCli.exec id "clean"
    |> ignore
)

Target.create "Build" (fun _ ->
    DotNetCli.build id slnFile
)

Target.create "Test" (fun _ ->
    sprintf
        "--project %s"
        (testDir </> "FSharpIL.Tests" </> "FSharpIL.Tests.fsproj")
    |> DotNetCli.exec id "run"
    |> handleErr "One or more tests failed"
)

Target.create "Lint" (fun _ ->
    slnFile
    |> sprintf "lint %s"
    |> DotNetCli.exec id "fsharplint"
    |> handleErr "One or more files is formatted incorrectly"
)

Target.create "Publish" (fun _ ->
    Trace.trace "Publishing..."
)

"Clean"
==> "Build"
==> "Test"
==> "Lint"
==> "Publish"

Target.runOrDefault "Publish"
