namespace FSharpIL

open BenchmarkDotNet.Attributes

open System
open System.Collections.Immutable

open FSharpIL
open FSharpIL.PortableExecutable

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.Metadata.Unchecked
open FSharpIL.Metadata.UncheckedExn

[<StatisticalTestColumn>]
[<MemoryDiagnoser>]
type ToArrayBenchmarks () =
    [<Benchmark>] member _.HelloWorld() = HelloWorld.example() |> WritePE.toArray
    [<Benchmark>] member _.CustomCollections() = CustomCollections.example() |> WritePE.toArray
