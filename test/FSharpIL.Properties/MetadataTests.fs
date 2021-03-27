module FSharpIL.MetadataTests

open Expecto

open Swensen.Unquote

open System
open System.Collections.Immutable

open Mono.Cecil

open FSharpIL.Generate

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.PortableExecutable

let testCecil name body =
    testProperty name <| fun (ValidAssembly pe) ->
        use mdle = ModuleDefinition.ReadModule(WritePE.stream pe)
        body pe mdle

[<Tests>]
let tests =
    testList "metadata" [
        testCecil "module name matches parsed name" <| fun pe mdle ->
            let expected = string pe.CliHeader.Value.Module.Name
            expected =! mdle.Name

        testCecil "names of defined types match parsed names" <| fun pe mdle ->
            let expected =
                pe.CliHeader.Value.TypeDef.Rows
                |> Seq.map (fun t -> string t.TypeName, t.TypeNamespace)
                |> List.ofSeq
            let actual =
                mdle.Types
                |> Seq.map (fun t -> t.Name, t.Namespace)
                |> List.ofSeq
            expected =! actual
    ]
