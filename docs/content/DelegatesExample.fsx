(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.DelegatesExample

open Expecto

open Swensen.Unquote

open System.IO

open FSharpIL
#endif
(**
# Delegates Example

The following example showcases the generation and usage of delegate types.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Unchecked
open FSharpIL.Metadata.UncheckedExn
open FSharpIL.Metadata.CliMetadata
open FSharpIL.PortableExecutable

let example() =
    let builder =
        { Name = Identifier.ofStr "DelegatesExample.netmodule"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    let struct (mscorlib, _) =
        { Version = Version(5, 0, 0, 0)
          PublicKeyOrToken = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
          Name = AssemblyName.ofStr "System.Runtime"
          Culture = NullCulture
          HashValue = None }
        |> referenceAssembly builder
    let object =
        { TypeName = Identifier.ofStr "Object"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder
    let mcdelegate =
        { TypeName = Identifier.ofStr "MulticastDelegate"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder
    let aresult =
        { TypeName = Identifier.ofStr "IAsyncResult"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder
    let acallback =
        { TypeName = Identifier.ofStr "AsyncCallback"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder

    (*Generating Delegate Types*)
    // type MyDelegate = delegate of string * uint32 -> string
    let mydel =
        let parameters = Array.map ParamItem.create [| EncodedType.String; EncodedType.U4 |]
        UncheckedExn.Unsafe.AddDelegate (
            builder,
            mcdelegate,
            EncodedType.Class(TypeDefOrRefOrSpecEncoded.TypeRef aresult),
            EncodedType.Class(TypeDefOrRefOrSpecEncoded.TypeRef acallback),
            DelegateDef (
                TypeVisibility.Public,
                ReturnType.encoded EncodedType.String,
                ImmutableArray.CreateRange parameters,
                Identifier.ofStr "MyDelegate"
            )
        )
 
    (*Using Delegate Types*)

    // type MyClass
    let myclass =
        { Access = TypeVisibility.Public
          Flags = ClassFlags(AutoLayout, AnsiClass, beforeFieldInit = true) |> Flags.concreteClass
          ClassName = Identifier.ofStr "MyClass"
          TypeNamespace = String.Empty
          Extends = Extends.TypeRef object }
        |> ConcreteClass.addTypeDef builder

    // static member DuplicateString(str: string, times: uint32): string
    let dupstr =
        ()

    CliMetadata builder |> PEFile.ofMetadata ImageFileFlags.dll

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    let example' = lazy example()

    testList "delegates example" [
        testCase "can save to disk" <| fun() ->
            let path = Path.Combine(__SOURCE_DIRECTORY__, "exout", "DelegatesExample.netmodule")
            WritePE.toPath path example'.Value
    ]
#endif
