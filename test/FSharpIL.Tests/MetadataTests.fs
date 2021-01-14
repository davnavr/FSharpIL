﻿module FSharpIL.MetadataTests

open Expecto

open System
open System.Reflection

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
                "the byte representation of the metadata version should match"

        testList "computation expression" [
            testCase "variable can be used" <| fun() ->
                let expected =
                    { Version = Version(2, 0, 0, 0)
                      Flags = ()
                      PublicKeyOrToken = PublicKeyOrToken.NoPublicKey
                      Name = AssemblyName.ofStr "mscorlib"
                      Culture = AssemblyCulture.NullCulture
                      HashValue = None }
                let metadata =
                    metadataBuilder {
                        let! token = assemblyRef expected
                        typeRef
                            { ResolutionScope = ResolutionScope.AssemblyRef token
                              TypeName = NonEmptyName.ofStr "Test"
                              TypeNamespace = "" }
                    }
                    |> ValidationResult.get
                let actual =
                    let (KeyValue (testType, _)) = metadata.TypeRef |> Seq.head
                    match testType.Item.ResolutionScope with
                    | ResolutionScope.AssemblyRef assm -> assm.Item
                    | _ -> Unchecked.defaultof<_>
                Expect.equal actual expected "the assembly references should match"

            testCase "error skips rest of expression" <| fun() ->
                let mutable run = false
                metadataBuilder {
                    fun _ -> MissingType("", Unchecked.defaultof<NonEmptyName>) |> Error
                    run <- true
                }
                |> ignore
                Expect.isFalse run "an error should mean that the rest of the expression should not be evaluated"

            testCase "struct definition results in error when System.ValueType is missing" <| fun() ->
                let result =
                    metadataBuilder {
                        structDef
                            { Access = TypeVisibility.Public
                              Flags = StructFlags.Zero
                              StructName = NonEmptyName.ofStr "MyStruct"
                              TypeNamespace = "Testing"
                              FieldList = ()
                              MethodList = () }
                    }
                ValidationExpect.isError result "result should be error when System.ValueType cannot be found"

            testCase "class flags are declared correctly" <| fun() ->
                let metadata =
                    metadataBuilder {
                        classDef
                            { Access = TypeVisibility.Public
                              Flags =
                                { ClassFlags.Zero with
                                    BeforeFieldInit = true
                                    StringFormat = UnicodeClass }
                                |> ConcreteClassFlags
                              ClassName = NonEmptyName.ofStr "MyClass"
                              TypeNamespace = ""
                              Extends = Extends.Null
                              FieldList = ()
                              MethodList = () }
                    }
                    |> ValidationResult.get
                let (KeyValue (def, _)) = metadata.TypeDef |> Seq.head
                Expect.equal
                    def.Item.Flags
                    (TypeAttributes.UnicodeClass ||| TypeAttributes.BeforeFieldInit ||| TypeAttributes.Public)
                    "flags should match"

            testCase "structs are sealed" <| fun() ->
                let metadata =
                    metadataBuilder {
                        let! mscorlib =
                            assemblyRef
                                { Version = Version(5, 0, 0, 0)
                                  Flags = ()
                                  PublicKeyOrToken = NoPublicKey
                                  Name = AssemblyName.ofStr "System.Private.CoreLib"
                                  Culture = NullCulture
                                  HashValue = None }
                        typeRef
                            { ResolutionScope = ResolutionScope.AssemblyRef mscorlib
                              TypeName = NonEmptyName.ofStr "ValueType"
                              TypeNamespace = "System" }
                        structDef
                            { Access = TypeVisibility.NotPublic
                              Flags = StructFlags { ClassFlags.Zero with BeforeFieldInit = true }
                              StructName = NonEmptyName.ofStr "Thing"
                              TypeNamespace = "Thing"
                              FieldList = ()
                              MethodList = () }
                    }
                    |> ValidationResult.get
                let (KeyValue (def, _)) = metadata.TypeDef |> Seq.head
                Expect.equal
                    (def.Item.Flags &&& TypeAttributes.Sealed)
                    TypeAttributes.Sealed
                    "user-defined value types should always have the Sealed flag"
        ]
    ]
