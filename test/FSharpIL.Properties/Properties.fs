module FSharpIL.Properties

open Expecto

open Swensen.Unquote

open System
open System.Collections.Immutable
open System.Reflection.PortableExecutable

open Mono.Cecil

open FSharpIL.Generate

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.PortableExecutable

let testCecil name body =
    testProperty name <| fun (ValidAssembly pe) ->
        use mdle = ModuleDefinition.ReadModule(WritePE.stream pe)
        body pe mdle

let testPE name body =
    testProperty name <| fun (ValidAssembly pe) ->
        use reader = new PEReader(WritePE.stream pe)
        body pe reader

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

//[<Tests>]
//let tests =
//    testList "write PE" [
//        testPE "section names match parsed name" <| fun pe reader ->
//            let expected =
//                pe.SectionTable
//                |> Seq.map (fun section -> string section.Header.SectionName)
//                |> List.ofSeq
//            let actual =
//                reader.PEHeaders.SectionHeaders
//                |> Seq.map (fun header -> header.Name)
//                |> List.ofSeq
//            expected =! actual

//        testPE "file alignment matches" <| fun pe reader ->
//            reader.PEHeaders.PEHeader.FileAlignment =! int32 pe.NTSpecificFields.FileAlignment

//        testPE "all PE files are PE32" <| fun _ reader ->
//            reader.PEHeaders.PEHeader.Magic =! PEMagic.PE32

//        testPE "all PE files have 16 data directories" <| fun _ reader ->
//            reader.PEHeaders.PEHeader.NumberOfRvaAndSizes =! 0x10
//    ]
