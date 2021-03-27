module FSharpIL.WritePETests

open Expecto

open Swensen.Unquote

open System.Reflection.PortableExecutable

open FSharpIL.Generate

open FSharpIL.PortableExecutable

let testPE name body =
    testProperty name <| fun (ValidAssembly pe) ->
        use reader = new PEReader(WritePE.stream pe)
        body pe reader

[<Tests>]
let tests =
    testList "write PE" [
        testPE "section names match parsed name" <| fun pe reader ->
            let expected =
                pe.SectionTable
                |> Seq.map (fun section -> string section.Header.SectionName)
                |> List.ofSeq
            let actual =
                reader.PEHeaders.SectionHeaders
                |> Seq.map (fun header -> header.Name)
                |> List.ofSeq
            expected =! actual

        testPE "file alignment matches" <| fun pe reader ->
            reader.PEHeaders.PEHeader.FileAlignment =! int32 pe.NTSpecificFields.FileAlignment

        testPE "all PE files are PE32" <| fun _ reader ->
            reader.PEHeaders.PEHeader.Magic =! PEMagic.PE32

        testPE "all PE files have 16 data directories" <| fun _ reader ->
            reader.PEHeaders.PEHeader.NumberOfRvaAndSizes =! 0x10
    ]
