(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.ClassesExample

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
# Classes Example

The following example showcases the generation of classes as well as the various opcodes used to interact with objects.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.Metadata.SystemAssembly
open FSharpIL.PortableExecutable

let example() =
    metadata {
        let ns = "FSharpIL.Examples"
        let! assm =
            setAssembly
                { Name = sprintf "%s.Classes" ns |> AssemblyName.ofStr
                  HashAlgId = ()
                  Version = Version(1, 0, 0, 0)
                  Flags = ()
                  PublicKey = None
                  Culture = NullCulture }

        let! mscorlib = Net5_0.private_corelib
        let! object = SystemTypes.object mscorlib

        // Represents a list of names.
        let! nameList =
            addSealedClass
                { Access = TypeVisibility.Public
                  Flags = Flags.sealedClass ClassFlags.None
                  ClassName = Identifier.ofStr "NameList"
                  TypeNamespace = ns
                  Extends = Extends.TypeRef object }

        let! names =
            { FieldName = Identifier.ofStr "names"
              Flags =
                { Visibility = Visibility.Private
                  NotSerialized = false
                  SpecialName = false }
                |> Flags.instanceField
              Signature = EncodedType.SZArray(ImmutableArray.Empty, EncodedType.String) |> FieldSignature.create }
            |> InstanceField
            |> addField nameList

        ()
    }
    |> CliMetadata.createMetadata
        { Name = Identifier.ofStr "FSharpIL.Examples.Classes.exe"
          Mvid = Guid.NewGuid() }
    |> ValidationResult.get
    |> PEFile.ofMetadata ImageFileFlags.exe
(*** hide ***)
#if !COMPILED
[<Tests>]
let tests =
    let testCaseCecil name test =
        fun() ->
            use metadata = example() |> WritePE.stream |> ModuleDefinition.ReadModule
            test metadata
        |> testCase name

    testList "Classes Example" [
        testCase "temporary name ___________" <| fun() ->
            let output = Path.Combine(__SOURCE_DIRECTORY__, "tmp")
            let executable = Path.Combine(output, "FSharpIL.Examples.Classes.dll")

            example() |> WritePE.toPath executable
    ]
#endif
