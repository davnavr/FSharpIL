module FSharpIL.MetadataTests

open Expecto

open System
open System.Collections.Immutable

open Mono.Cecil

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
                { Body =
                    MethodBody.create <| fun content ->
                        let writer = MethodBodyWriter content
                        writer.Ret()
                  ImplFlags = MethodImplFlags.None
                  Flags = Flags.staticMethod { Visibility = Visibility.Public; HideBySig = true }
                  MethodName = Identifier.ofStr "Main"
                  Signature = StaticMethodSignature(MethodCallingConventions.Default, ReturnTypeItem.Void, ImmutableArray.Empty)
                  ParamList = fun _ -> failwith "no parameters" }

            let pe =
                metadata {
                    let! program =
                        buildStaticClass
                            { Access = TypeVisibility.Public
                              Extends = Extends.Null
                              ClassName = Identifier.ofStr "Program"
                              TypeNamespace = ""
                              Flags = Flags.staticClass ClassFlags.None }

                    let main =
                        IndexedList.add
                            (StaticClassMethod.Method entrypoint)
                            program.Methods
                        |> ValueOption.get

                    program.BuildType |> ignore

                    do! setEntrypoint main
                }
                |> createMetadata
                    { Mvid = Guid.NewGuid()
                      Name = Identifier.ofStr "Program.exe" }
                |> ValidationResult.get
                |> PEFile.ofMetadata IsExe

            use metadata = WritePE.stream pe |> ModuleDefinition.ReadModule
            Expect.equal metadata.EntryPoint.Name (string entrypoint.MethodName) "name of entrypoint should match"

        testCase "error skips rest of metadata expression" <| fun() ->
            let error = MissingTypeError("test", Identifier.ofStr "test") :> ValidationError
            let mutable skipped = true

            let result =
                metadata {
                    do! fun () -> Error error
                    skipped <- false
                }
                |> CliMetadata.createMetadata
                    { Mvid = Guid.NewGuid()
                      Name = Identifier.ofStr "Empty" }

            ValidationExpect.isSpecificError result error "expression should evaluate to an error"
            Expect.isTrue skipped "rest of expression should not be evaluated if an error occurs"
    ]
