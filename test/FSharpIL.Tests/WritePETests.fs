module FSharpIL.WritePETests

open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable

open Expecto

open FSharpIL.PortableExecutable

[<Tests>]
let tests =
    let inline testPE name pe body =
        testCase name <| fun() ->
            let data = WritePE.toArray pe
            use source = new MemoryStream(data, false)
            use reader = new PEReader(source, PEStreamOptions.PrefetchEntireImage)
            body reader |> ignore
    let inline testMetadata name pe body =
        testPE name pe <| fun reader ->
            reader.GetMetadataReader() |> body

    testList "write PE" [
        testPE "default PE has metadata" PEFile.Default <| fun reader ->
            Expect.isTrue reader.HasMetadata "Generated PE should contain metadata"

        testPE "default PE sections have correct names" PEFile.Default <| fun reader ->
            let names =
                reader.PEHeaders.SectionHeaders
                |> Array.ofSeq
                |> Array.map (fun header -> header.Name)
            Expect.sequenceEqual
                names
                [| ".text"; ".rsrc"; ".reloc" |]
                "Default section headers are missing one or more sections"

        testMetadata "default metadata is an assembly" PEFile.Default <| fun metadata ->
            Expect.isTrue metadata.IsAssembly "Generated CLI metadata should be an assembly"
    ]
