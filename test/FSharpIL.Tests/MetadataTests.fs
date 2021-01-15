module FSharpIL.MetadataTests

open Expecto

open System
open System.Reflection

open FSharpIL.Metadata

[<Tests>]
let tests =
    testList "metadata" [
        ftestList "version" [
            let testVersion name body =
                testPropertyWithConfig
                    Generate.config
                    name
                    (fun (VersionString ver) -> body ver)

            testVersion "byte length is a multiple of 4" <| fun ver ->
                (MetadataVersion.ofStr ver |> MetadataVersion.toArray |> Array.length) % 4 = 0

            testVersion "byte length is greater than or equal to string length" <| fun ver ->
                let bytes = MetadataVersion.ofStr ver |> MetadataVersion.toArray
                bytes.Length >= ver.Length

            testVersion "string representation as version equals original" <| fun ver ->
                let ver' = MetadataVersion.ofStr ver
                (string ver' |> MetadataVersion.ofStr) = ver'

            testVersion "version as string matches original string" <| fun expected ->
                let ver = MetadataVersion.ofStr expected
                let actual = string ver
                actual = expected

            testVersion "last byte of version string bytes is null" <| fun ver ->
                (MetadataVersion.ofStr ver |> MetadataVersion.toArray |> Array.last) = 0uy
        ]

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
                        TypeRef.Add
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
                        TypeDef.AddStruct
                            { Access = TypeVisibility.Public
                              Flags = StructFlags.Zero
                              StructName = NonEmptyName.ofStr "MyStruct"
                              TypeNamespace = "Testing"
                              Fields = FieldSet()
                              Methods = () }
                    }
                ValidationExpect.isError result "result should be error when System.ValueType cannot be found"

            testCase "class flags are declared correctly" <| fun() ->
                let metadata =
                    metadataBuilder {
                        TypeDef.AddClass
                            { Access = TypeVisibility.Public
                              Flags =
                                { ClassFlags.Zero with
                                    BeforeFieldInit = true
                                    StringFormat = UnicodeClass }
                                |> ConcreteClassFlags
                              ClassName = NonEmptyName.ofStr "MyClass"
                              TypeNamespace = ""
                              Extends = Extends.Null
                              Fields = FieldSet()
                              Methods = () }
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
                        TypeRef.Add
                            { ResolutionScope = ResolutionScope.AssemblyRef mscorlib
                              TypeName = NonEmptyName.ofStr "ValueType"
                              TypeNamespace = "System" }
                        TypeDef.AddStruct
                            { Access = TypeVisibility.NotPublic
                              Flags = StructFlags { ClassFlags.Zero with BeforeFieldInit = true }
                              StructName = NonEmptyName.ofStr "Thing"
                              TypeNamespace = "Thing"
                              Fields = FieldSet()
                              Methods = () }
                    }
                    |> ValidationResult.get
                let (KeyValue (def, _)) = metadata.TypeDef |> Seq.head
                Expect.equal
                    (def.Item.Flags &&& TypeAttributes.Sealed)
                    TypeAttributes.Sealed
                    "user-defined value types should always have the Sealed flag"
        ]
    ]
