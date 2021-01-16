module FSharpIL.WritePETests

open Expecto

open System
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
                        { Assembly.Name = AssemblyName.ofStr "HelloWorld"
                          HashAlgId = ()
                          Version = Version()
                          Flags = ()
                          PublicKey = None
                          Culture = NullCulture }
                    let! mscorlib =
                        AssemblyRef.Add
                            { Version = Version(5, 0, 0, 0)
                              Flags = AssemblyNameFlags.PublicKey
                              PublicKeyOrToken = Array.map byte [| 0x7c; 0xec; 0x85; 0xd7; 0xbe; 0xa7; 0x79; 0x8e |] |> PublicKey
                              Name = AssemblyName.ofStr "System.Private.CoreLib"
                              Culture = NullCulture
                              HashValue = None } // TODO: What hash value?
                    let! console =
                        AssemblyRef.Add
                            { Version = Version(5, 0, 0, 0)
                              Flags = AssemblyNameFlags.PublicKey
                              PublicKeyOrToken = Array.map byte [| 0xb0; 0x3f; 0x5f; 0x7f; 0x11; 0xd5; 0x0a; 0x3a |] |> PublicKey
                              Name = AssemblyName.ofStr "System.Console"
                              Culture = NullCulture
                              HashValue = None } // TODO: What hash value?
                    let! object =
                        TypeRef.Add
                            { TypeName = Identifier.ofStr "Object"
                              TypeNamespace = "System"
                              ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
                    TypeDef.AddClass
                        { Access = TypeVisibility.Public
                          ClassName = Identifier.ofStr "Program"
                          Extends = Extends.TypeRef object
                          Fields = FieldSet()
                          Flags = StaticClassFlags ClassFlags.Zero 
                          TypeNamespace = "HelloWorld"
                          Methods = MethodSet() } // TODO: Add methods
                }
                |> ValidationResult.get
            let pe = CliHeader.Default tables |> PEFile.ofMetadata IsExe
            ()
    ]
