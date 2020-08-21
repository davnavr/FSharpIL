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

Target.create "Clean" (fun _ ->
    Trace.trace "Cleaning..."
)

Target.create "Build" (fun _ ->
    Trace.trace "Building..."
)

Target.create "Test" (fun _ ->
    Trace.trace "Testing..."
)

Target.create "Lint" (fun _ ->
    Trace.trace "Linting..."
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
