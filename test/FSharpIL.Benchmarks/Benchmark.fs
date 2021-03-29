module FSharpIL.Benchmark

open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    let assm = System.Reflection.Assembly.GetExecutingAssembly()
    BenchmarkSwitcher
        .FromAssembly(assm)
        .Run(args = argv)
    |> ignore
    0
