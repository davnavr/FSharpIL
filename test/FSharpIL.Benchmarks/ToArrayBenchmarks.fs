namespace FSharpIL

open BenchmarkDotNet.Attributes

open FSharpIL

[<StatisticalTestColumn>]
[<MemoryDiagnoser>]
type ToArrayBenchmarks () =
    [<Benchmark>] member _.HelloWorld() = HelloWorld.example() |> WritePE.toArray
    [<Benchmark>] member _.CustomCollections() = CustomCollections.example() |> WritePE.toArray
