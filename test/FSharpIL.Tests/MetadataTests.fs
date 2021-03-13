﻿module FSharpIL.MetadataTests

open Expecto

open System
open System.Collections.Immutable

open Mono.Cecil

open FSharpIL.Generate

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.PortableExecutable

[<Tests>]
let tests =
    testList "metadata" [
        let testAssembly name body =
            testProperty name <| fun (ValidAssembly pe) ->
                use mdle = ModuleDefinition.ReadModule(WritePE.stream pe)
                body pe mdle

        testAssembly "module name matches parsed name" <| fun pe mdle ->
            let expected = string pe.CliHeader.Value.Module.Name
            expected = mdle.Name

        testAssembly "names of defined types match parsed names" <| fun pe mdle ->
            let expected =
                pe.CliHeader.Value.TypeDef.Rows |> Seq.map (fun t -> string t.TypeName, t.TypeNamespace)
            let actual =
                mdle.Types |> Seq.map (fun t -> t.Name, t.Namespace)
            Expect.sequenceEqual actual expected "type name and namespace should match"
    ]
