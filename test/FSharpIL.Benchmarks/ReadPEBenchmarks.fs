namespace FSharpIL

open BenchmarkDotNet.Attributes

[<Struct; System.Runtime.CompilerServices.IsReadOnly>]
type AssemblyPath =
    | Assembly of System.Reflection.Assembly

    member this.Location =
        match this with
        | Assembly assm -> assm.Location

    override this.ToString() =
        match this with
        | Assembly assm -> assm.GetName().Name

[<MemoryDiagnoser>]
type ReadPEBenchmarks() =
    member _.StreamSource =
        Array.map
            (fun (t: System.Type) -> Assembly t.Assembly)
            [|
                typeof<BenchmarkAttribute>
                typeof<PortableExecutable>
                typeof<System.Collections.Immutable.IImmutableList<obj>>
            |]

    [<Benchmark; ArgumentsSource("StreamSource")>]
    member _.FromPath(path: AssemblyPath) = ReadPE.fromPath path.Location ()
