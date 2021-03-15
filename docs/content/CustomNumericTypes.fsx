(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.CustomNumericTypes

open Expecto

open Swensen.Unquote

open System.IO

open FSharpIL
#endif
(**
# Custom Numeric Types

The following example showcases the generation of structs and implementation of interfaces by creating custom numeric types.
Instead of the usual functions that can check for CLS violations and produce warnings, this example uses functions that skip all
checks and throw exceptions on errors instead.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.Metadata.CliMetadata.Unchecked
open FSharpIL.PortableExecutable

let example() =
    let builder =
        { Name = Identifier.ofStr "CustomNumbers.dll"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    // TODO: Make versions of SystemAssembly and SystemType modules that don't use warning builders.
    let throwaway = ImmutableArray.CreateBuilder<_>()

    let mscorlib = SystemAssembly.Net5_0.private_corelib builder throwaway
    let valueType = SystemType.valueType builder mscorlib
    let icomparable_1 =
        { TypeName = Identifier.ofStr "IComparable`1"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder

    // TODO: In the future, this would be a good example to showcase functions to generate XML documentation.
    let nonNegInt = // TODO: Use helper function to define a struct instead.
        { StructDef.Access = TypeVisibility.Public
          Flags = failwith "TODO: Get struct flags"
          StructName = Identifier.ofStr "NonNegInt"
          TypeNamespace = "CustomNumbers" }
        |> addStruct builder



    // setTargetFramework

    CliMetadata builder |> PEFile.ofMetadata ImageFileFlags.dll

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    let example' =
        lazy
            try example()
            with
            | ValidationErrorException err as ex -> raise(Exception(err.ToString(), ex))
    let metadata = lazy PEFile.toCecilModule example'.Value
    let context =
        lazy
            let (ctx, assembly) = PEFile.toLoadContext "factorial" example'.Value
            {| Context = ctx; Assembly = assembly |}

    afterRunTests <| fun() ->
        context.Value.Context.Unload()
        metadata.Value.Dispose()

    testList "custom numeric types" [
    ]
#endif
