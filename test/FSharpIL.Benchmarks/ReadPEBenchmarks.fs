namespace FSharpIL

open BenchmarkDotNet.Attributes

[<Struct; System.Runtime.CompilerServices.IsReadOnly>]
type AssemblyPath = AssemblyPath of string

[<MemoryDiagnoser>]
type ReadPEBenchmarks() =
    member _.StreamSource =
        Array.map
            (fun (t: System.Type) -> AssemblyPath t.Assembly.Location)
            [|
                typeof<BenchmarkAttribute>
                typeof<PortableExecutable>
            |]

    [<Benchmark; ArgumentsSource("StreamSource")>]
    member _.FromPath(AssemblyPath path) = ReadPE.fromPath path ()
