module FSharpIL.WritePETests

open System.IO
open System.Reflection.PortableExecutable

open Expecto

open FSharpIL.PortableExecutable

[<Tests>]
let tests =
    let testPE name body =
        testCase name <| fun() ->
            let data =
                use str = new MemoryStream(512)
                do WritePE.toStream str PEFile.Default
                str.GetBuffer()
            use source = new MemoryStream(data, false)
            use reader = new PEReader(source, PEStreamOptions.PrefetchEntireImage)
            body reader |> ignore

    testList "write PE" [
        testPE "default PE has metadata" <| fun reader ->
            Expect.isTrue reader.HasMetadata "Generated PE should contain metadata"

        testPE "default PE sections have correct names" <| fun reader ->
            let names =
                reader.PEHeaders.SectionHeaders
                |> Array.ofSeq
                |> Array.map (fun header -> header.Name)
            Expect.sequenceEqual
                names
                [| ".text"; ".rsrc"; ".reloc" |]
                "Default section headers are missing one or more sections"
    ]
