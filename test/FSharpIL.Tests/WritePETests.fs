module FSharpIL.WritePETests

open Expecto

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

[<Tests>]
let tests =
    let testPE name pe body =
        testCase name <| fun() ->
            let data = WritePE.toArray pe
            use source = new MemoryStream(data, false)
            use reader = new PEReader(source, PEStreamOptions.PrefetchEntireImage)
            body reader |> ignore
    let testMetadata name pe body =
        testPE name pe <| fun reader ->
            reader.GetMetadataReader() |> body

    testList "write PE" [
        //testPE "default PE sections have correct names" PEFile.Default <| fun reader ->
        //    let names =
        //        reader.PEHeaders.SectionHeaders
        //        |> Array.ofSeq
        //        |> Array.map (fun header -> header.Name)
        //    Expect.sequenceEqual
        //        names
        //        [| ".text"; ".rsrc"; ".reloc" |]
        //        "default section headers are missing one or more sections"

        //testMetadata "default metadata is an assembly" PEFile.Default <| fun metadata ->
        //    Expect.isTrue metadata.IsAssembly "generated CLI metadata should be an assembly"

        testProperty "hello world is an assembly" <| fun mvid ->
            let mdle =
                { Name = Identifier.ofStr "HelloWorld"
                  Mvid = mvid }
            let tables =
                metadataBuilder mdle {
                    assembly
                        { Name = AssemblyName.ofStr "HelloWorld"
                          HashAlgId = ()
                          Version = Version()
                          Flags = ()
                          PublicKey = None
                          Culture = NullCulture }
                    let! mscorlib =
                        AssemblyRef.Add
                            { Version = Version(5, 0, 0, 0)
                              PublicKeyOrToken = PublicKeyToken(0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
                              Name = AssemblyName.ofStr "System.Private.CoreLib"
                              Culture = NullCulture
                              HashValue = None }
                    let! console =
                        AssemblyRef.Add
                            { Version = Version(5, 0, 0, 0)
                              PublicKeyOrToken = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
                              Name = AssemblyName.ofStr "System.Console"
                              Culture = NullCulture
                              HashValue = None }
                    let! object =
                        TypeRef.Add
                            { TypeName = Identifier.ofStr "Object"
                              TypeNamespace = "System"
                              ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
                    let! methodList =
                        methods {
                            StaticClassMethod.Method
                                { Body = invalidOp "Body?"
                                  ImplFlags = MethodImplFlags.Zero
                                  MethodName = Identifier.ofStr "Method"
                                  Flags =
                                    { Visibility = Public
                                      HideBySig = true }
                                    |> StaticMethodFlags
                                  Signature =
                                    let args =
                                        { CustomMod = ImmutableArray.Empty
                                          ParamType = EncodedType.Array(EncodedType.String, ArrayShape.OneDimension) }
                                        |> ImmutableArray.Create
                                    StaticMethodSignature(MethodCallingConventions.Default, ReturnTypeItem.Void, args)
                                  ParamList = fun _ _ -> Param { Flags = ParamFlags.Zero; ParamName = "args" } }
                        }
                    TypeDef.AddClass
                        { Access = TypeVisibility.Public
                          ClassName = Identifier.ofStr "Program"
                          Extends = Extends.TypeRef object
                          Fields = FieldList.Empty
                          Flags = StaticClassFlags ClassFlags.Zero 
                          TypeNamespace = "HelloWorld"
                          Methods = methodList }
                }
                |> ValidationResult.get
            let pe = CliHeader.Default tables |> PEFile.ofMetadata IsExe
            ()
    ]
