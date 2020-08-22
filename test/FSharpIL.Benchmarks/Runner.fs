module FSharpIL.Runner

open BenchmarkDotNet.Running

[<EntryPoint>]
let main args =
    let assm = System.Reflection.Assembly.GetExecutingAssembly()
    let summaries = BenchmarkSwitcher.FromAssembly(assm).Run(args)
    let success =
        summaries
        |> Seq.collect (fun summary -> summary.Reports)
        |> Seq.forall (fun report -> report.Success)
    if success then 0 else -1
