module FSharpIL.WritePETests

open FsCheck

open Expecto

open System.Collections.Immutable
open System.Reflection.PortableExecutable

open FSharpIL.PortableExecutable

type Generate() =
    static member Alignment() =
        gen {
            let! falignment = Gen.choose(9, 14)
            let! salignment = Gen.choose(falignment + 1, 15)
            let inline pow2 num = pown 2 num
            return Alignment.create (pow2 salignment) (pow2 falignment) |> Option.get
        }
        |> Arb.fromGen

    static member PESections() =
        gen {
            let! sections =
                gen {
                    let! data =
                        Arb.generate<byte[]>
                        |> Gen.map (fun data -> RawData(fun() -> data))
                        |> Gen.arrayOf
                    let! kind = Arb.generate

                    return
                        { Data = ImmutableArray.Create(items = data)
                          Kind = kind }
                }
                |> Gen.arrayOf
            return ImmutableArray.Create(items = sections) |> PESections
        }
        |> Arb.fromGen

[<Tests>]
let tests =
    let config =
        { FsCheckConfig.defaultConfig with
            arbitrary = typeof<Generate> :: FsCheckConfig.defaultConfig.arbitrary }

    let inline testProperty name = testPropertyWithConfig config name

    let testPE name body =
        testProperty name <| fun (pe: PEFile) ->
            use reader = new PEReader(WritePE.stream pe)
            body pe reader

    testList "write PE" [
        testPE "section names match parsed name" <| fun pe reader ->
            let expected =
                pe.SectionTable |> Seq.map (fun section -> string section.Header.SectionName)
            let actual =
                reader.PEHeaders.SectionHeaders |> Seq.map (fun header -> header.Name)
            Expect.sequenceEqual actual expected "section name should match"

        testPE "file alignment matches" <| fun pe reader ->
            reader.PEHeaders.PEHeader.FileAlignment = int32 pe.NTSpecificFields.FileAlignment

        testPE "all PE files are PE32" <| fun _ reader ->
            reader.PEHeaders.PEHeader.Magic = PEMagic.PE32

        testPE "all PE files have 10 data directories" <| fun _ reader ->
            reader.PEHeaders.PEHeader.NumberOfRvaAndSizes = 10
    ]
