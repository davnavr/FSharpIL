(*** hide ***)
#if DOCUMENTATION
#r "../Dependencies.fsx"
#else
module FSharpIL.FieldsExample

open Expecto

open Swensen.Unquote

open System.Diagnostics
open System.IO

open Newtonsoft.Json
open Newtonsoft.Json.Linq

open Mono.Cecil

open FSharpIL
#endif
(**
# Fields Example

The following example showcases the generation of fields as well as the usage of various opcodes to load the values of fields
and to store values into fields.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.PortableExecutable

let example() =
    metadata {
        let! assm =
            setAssembly
                { Name = AssemblyName.ofStr "Example.Fields"
                  HashAlgId = ()
                  Version = Version(1, 0, 0, 0)
                  Flags = ()
                  PublicKey = None
                  Culture = NullCulture }

        // TODO: Figure out how to avoid copy and pasting references to System assemblies. Consider providing a module that contains commonly used AssemblyReferences and their TypeReferences.
        let! mscorlib =
            referenceAssembly
                { Version = Version(5, 0, 0, 0)
                  PublicKeyOrToken = PublicKeyToken(0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
                  Name = AssemblyName.ofStr "System.Private.CoreLib"
                  Culture = NullCulture
                  HashValue = None }

        let! object =
            referenceType
                { TypeName = Identifier.ofStr "Object"
                  TypeNamespace = "System"
                  ResolutionScope = ResolutionScope.AssemblyRef mscorlib }

        let! examples =
            buildStaticClass
                { Access = TypeVisibility.Public
                  ClassName = Identifier.ofStr "Program"
                  Extends = Extends.TypeRef object
                  Flags = Flags.staticClass { ClassFlags.None with BeforeFieldInit = true }
                  TypeNamespace = "HelloWorld" }

        // TODO: Do things with fields.

        let! examples' = examples.BuildType()

        ()
    }
    |> CliMetadata.createMetadata
        { Name = Identifier.ofStr "HelloWorld.exe"
          Mvid = Guid.NewGuid() }
    |> ValidationResult.get
    |> PEFile.ofMetadata ImageFileFlags.exe
(*** hide ***)
#if !DOCUMENTATION
[<Tests>]
let tests =
    testList "Fields Example" [
        testCase "has correct field names" <| fun() ->
            failwith "TODO: Add more tests"
    ]
#endif
