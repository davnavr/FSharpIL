(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.GenericsShowcase
#if !BENCHMARK
open Expecto

open Swensen.Unquote

open System.IO

open FSharpIL
#endif
#endif
(**
# Generics Showcase

The following example showcases the generation of generic parameters on classes and methods, as well as the usage of generic
types and members from other assemblies.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.PortableExecutable

open FSharpIL.Writing
open FSharpIL.Writing.Cil

let example() =
    let namespace' = Identifier.ofStr "GenericsShowcase"

    let builder =
        ModuleBuilder (
            name = Identifier.ofStr "GenericsShowcase.exe",
            assembly =
                { AssemblyDefinition.Version = AssemblyVersion(4us, 3us, 2us, 1us)
                  PublicKey = ImmutableArray.Empty
                  Name = FileName.ofId namespace'
                  Culture = ValueNone }
        )

    let mscorlib =
        CoreAssemblyReference.NetCore (
            version = AssemblyVersion(5us, 0us, 0us, 0us),
            publicKeyToken = (0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
        )

    validated {
        let! core = mscorlib.AddReferencesTo builder
        ()
    }

    failwith "bad"

(*** hide ***)
#if COMPILED && !BENCHMARK
[<Tests>]
let tests =
    let example' = lazy example()

    testList "generics showcase" [
    ]
#endif
