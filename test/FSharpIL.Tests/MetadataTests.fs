﻿module FSharpIL.MetadataTests

open Expecto

open System

open FSharpIL.Metadata

[<Tests>]
let tests =
    testList "metadata" [
        testCase "metadata version is valid" <| fun() ->
            let actual =
                MetadataVersion.ofString "v4.0.30319"
                |> Option.get
                |> MetadataVersion.toArray
            Expect.equal
                actual
                [| 0x76uy; 0x34uy; 0x2Euy; 0x30uy; 0x2Euy; 0x33uy; 0x30uy; 0x33uy; 0x31uy; 0x39uy; 0uy; 0uy |]
                "The byte representation of the metadata version should match"

        testList "computation expression" [
            testCase "variable can be used" <| fun() ->
                let expected =
                    { Version = Version(2, 0, 0, 0)
                      Flags = ()
                      PublicKeyOrToken = PublicKeyOrToken.NoPublicKey
                      Name = AssemblyName.ofStr "mscorlib" |> Option.get
                      Culture = AssemblyCulture.NullCulture
                      HashValue = None }
                let metadata =
                    metadataBuilder {
                        let! token = addAssemblyRef expected
                        addTypeRef
                            { ResolutionScope = ResolutionScope.AssemblyRef token
                              TypeName = "Test"
                              TypeNamespace = "" }
                    }
                    |> ValidationResult.get
                let actual =
                    let testType = metadata.TypeRef |> Seq.head
                    match testType.ResolutionScope with
                    | ResolutionScope.AssemblyRef assm -> assm.Item
                    | _ -> Unchecked.defaultof<_>
                Expect.equal actual expected "The assembly references should match"
        ]
    ]
