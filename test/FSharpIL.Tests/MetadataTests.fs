module FSharpIL.MetadataTests

open Expecto

open Mono.Cecil

open System
open System.Collections.Immutable

open FSharpIL.Generate

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.PortableExecutable

[<Tests>]
let tests =
    testList "metadata" [
        let testAssembly name body =
            testProperty name <| fun (ValidAssembly pe) ->
                use mdle = ModuleDefinition.ReadModule(WritePE.stream pe)
                body pe mdle

        testAssembly "module name matches parsed name" <| fun pe mdle ->
            let expected = string pe.CliHeader.Value.Module.Name
            expected = mdle.Name

        testAssembly "names of defined types match parsed names" <| fun pe mdle ->
            let expected =
                pe.CliHeader.Value.TypeDef.Items |> Seq.map (fun t -> string t.TypeName, t.TypeNamespace)
            let actual =
                mdle.Types |> Seq.map (fun t -> t.Name, t.Namespace)
            Expect.sequenceEqual actual expected "type name and namespace should match"

        testCase "entrypoint is set correctly" <| fun() ->
            let entrypoint =
                { Body = ImmutableArray.Create Opcode.Ret
                  ImplFlags = MethodImplFlags.Zero
                  Flags = StaticMethodFlags { Visibility = Visibility.Public; HideBySig = true }
                  MethodName = Identifier.ofStr "Main"
                  Signature = StaticMethodSignature(MethodCallingConventions.Default, ReturnTypeItem.Void, ImmutableArray.Empty)
                  ParamList = fun _ -> failwith "no parameters" }

            let pe =
                metadata {
                    let! (program: TypeHandle<_>) =
                        TypeDef.AddClass
                            { StaticClassDef.Access = TypeVisibility.Public
                              Extends = Extends.Null
                              ClassName = Identifier.ofStr "Program"
                              TypeNamespace = ""
                              Flags = StaticClassFlags ClassFlags.Zero
                              Fields = FieldList.Empty
                              Methods = methods { StaticClassMethod.Method entrypoint } |> Result.get }

                    do! selectEntrypoint (fun _ -> true) program
                }
                |> createMetadata
                    { Mvid = Guid.NewGuid()
                      Name = Identifier.ofStr "Program.exe" }
                |> ValidationResult.get
                |> PEFile.ofMetadata IsExe

            use metadata = WritePE.stream pe |> ModuleDefinition.ReadModule
            Expect.equal metadata.EntryPoint.Name (string entrypoint.MethodName) "name of entrypoint should match"

        (*
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
                        let! token = AssemblyRef.Add expected
                        TypeRef.Add
                            { ResolutionScope = ResolutionScope.AssemblyRef token
                              TypeName = Identifier.ofStr "Test"
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
                    fun _ -> MissingType("", Unchecked.defaultof<Identifier>) |> Error
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
                              StructName = Identifier.ofStr "MyStruct"
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
                              ClassName = Identifier.ofStr "MyClass"
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
                            AssemblyRef.Add
                                { Version = Version(5, 0, 0, 0)
                                  Flags = ()
                                  PublicKeyOrToken = NoPublicKey
                                  Name = AssemblyName.ofStr "System.Private.CoreLib"
                                  Culture = NullCulture
                                  HashValue = None }
                        TypeRef.Add
                            { ResolutionScope = ResolutionScope.AssemblyRef mscorlib
                              TypeName = Identifier.ofStr "ValueType"
                              TypeNamespace = "System" }
                        TypeDef.AddStruct
                            { Access = TypeVisibility.NotPublic
                              Flags = StructFlags { ClassFlags.Zero with BeforeFieldInit = true }
                              StructName = Identifier.ofStr "Thing"
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

            testCase "nested classes work" <| fun() ->
                let metadata =
                    metadataBuilder {
                        let! mscorlib =
                            AssemblyRef.Add
                                { Version = Version(5, 0, 0, 0)
                                  Flags = ()
                                  PublicKeyOrToken = NoPublicKey
                                  Name = AssemblyName.ofStr "System.Private.CoreLib"
                                  Culture = NullCulture
                                  HashValue = None }
                        let! object =
                            TypeRef.Add
                                { ResolutionScope = ResolutionScope.AssemblyRef mscorlib
                                  TypeName = Identifier.ofStr "Object"
                                  TypeNamespace = "System" }
                        let! parent =
                            TypeDef.AddClass
                                { Access = TypeVisibility.Public
                                  Extends = Extends.TypeRef object
                                  Flags = ConcreteClassFlags ClassFlags.Zero
                                  ClassName = Identifier.ofStr "Parent"
                                  TypeNamespace = ""
                                  Fields = FieldSet()
                                  Methods = () }
                        TypeDef.AddClass
                            { Access = TypeVisibility.NestedPublic parent.Handle
                              Extends = Extends.ConcreteClass parent
                              Flags = ConcreteClassFlags ClassFlags.Zero
                              ClassName = Identifier.ofStr "Nested"
                              TypeNamespace = ""
                              Fields = FieldSet()
                              Methods = () }
                    }
                    |> ValidationResult.get
                let nested = Seq.head metadata.NestedClass
                Expect.equal nested.NestedClass.Item.TypeName (Identifier.ofStr "Nested") "nested classes should have an entry in the NestedClass table"
        ]
        *)
    ]
