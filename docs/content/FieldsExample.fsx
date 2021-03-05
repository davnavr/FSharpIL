(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.FieldsExample

open Expecto

open Swensen.Unquote

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
                { Name = AssemblyName.ofStr "FSharpIL.Examples.Fields"
                  HashAlgId = ()
                  Version = Version(1, 0, 0, 0)
                  Flags = ()
                  PublicKey = None
                  Culture = NullCulture }

        let! mscorlib = SystemAssembly.Net5_0.private_corelib
        let! object = SystemTypes.object mscorlib

        let! examples =
            addStaticClass
                { Access = TypeVisibility.Public
                  ClassName = Identifier.ofStr "Fields"
                  Extends = Extends.TypeRef object
                  Flags = Flags.staticClass ClassFlags.None
                  TypeNamespace = "FSharpIL.Examples" }

        let! myStaticField =
            { FieldName = Identifier.ofStr "myStaticField"
              Flags =
                { Visibility = Visibility.Private
                  NotSerialized = false
                  SpecialName = false }
                |> Flags.staticField
              Signature = FieldSignature.create EncodedType.I4 }
            |> StaticField
            |> addField examples

        // TODO: Do things with fields.

        let! main =
            { Body =
                fun content ->
                    let writer = MethodBodyWriter content
                    writer.Ldfld myStaticField
                    writer.Ret()
                |> MethodBody.create
              ImplFlags = MethodImplFlags.None
              MethodName = Identifier.ofStr "Main"
              Flags = Flags.staticMethod { Visibility = Public; HideBySig = true }
              Signature = EntryPointSignature.exitNoArgs
              ParamList = fun _ _ -> failwith "bad" } // TODO: Create function that throws exception when called for ParamList.
            |> EntryPointMethod
            |> addMethod examples

        ()
    }
    |> CliMetadata.createMetadata
        { Name = Identifier.ofStr "FSharpIL.Examples.Fields.exe"
          Mvid = Guid.NewGuid() }
    |> ValidationResult.get
    |> PEFile.ofMetadata ImageFileFlags.exe
(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    testList "Fields Example" [
        testCaseCecil example "has correct field names" <| fun metadata ->
            let expected = [ "myStaticField" ]
            let actual =
                metadata.Types
                |> Seq.collect (fun tdef -> tdef.Fields)
                |> Seq.map (fun field -> field.Name)
                |> List.ofSeq
            test <@ expected = actual @>
    ]
#endif
