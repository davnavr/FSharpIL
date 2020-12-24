module FSharpIL.MetadataTests

open Expecto

open FSharpIL.Metadata

[<Tests>]
let tests =
    testList "metadata tests" [
        testCase "metadata version is valid" <| fun() ->
            let actual =
                MetadataVersion.ofString "v4.0.30319"
                |> Option.get
                |> MetadataVersion.toArray
            Expect.equal
                actual
                [| 0x76uy; 0x34uy; 0x2Euy; 0x30uy; 0x2Euy; 0x33uy; 0x30uy; 0x33uy; 0x31uy; 0x39uy; 0uy; 0uy |]
                "The byte representation of the metadata version should match"
    ]
