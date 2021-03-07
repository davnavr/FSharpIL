namespace FSharpIL

open System.Collections.Generic
open System.Runtime.Loader

[<Sealed>]
type ExampleAssemblyLoadContext (name) =
    inherit AssemblyLoadContext(name, isCollectible = true)

    let assemblies =
        AssemblyLoadContext.Default.Assemblies
        |> Seq.map (fun assm -> assm.GetName(), assm)
        |> readOnlyDict

    override _.Load name = assemblies.GetValueOrDefault name
