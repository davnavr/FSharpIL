module FSharpIL.WritePETests

open Expecto

open System
open System.Collections.Immutable
open System.IO
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
        testProperty "hello world is an assembly" <| fun mvid ->
            let mdle =
                { Name = Identifier.ofStr "HelloWorld.dll"
                  Mvid = mvid }
            let tables =
                metadataBuilder mdle {
                    let! assm =
                        Assembly.Set
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
                    let! consolelib =
                        AssemblyRef.Add
                            { Version = Version(5, 0, 0, 0)
                              PublicKeyOrToken = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
                              Name = AssemblyName.ofStr "System.Console"
                              Culture = NullCulture
                              HashValue = None }

                    let! console =
                        TypeRef.Add
                            { TypeName = Identifier.ofStr "Console"
                              TypeNamespace = "System"
                              ResolutionScope = ResolutionScope.AssemblyRef consolelib }
                    let! object =
                        TypeRef.Add
                            { TypeName = Identifier.ofStr "Object"
                              TypeNamespace = "System"
                              ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
                    let! tfmAttr =
                        TypeRef.Add
                            { TypeName = Identifier.ofStr "TargetFrameworkAttribute"
                              TypeNamespace = "System.Runtime.Versioning"
                              ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
                    let string = { CustomMod = ImmutableArray.Empty; ParamType = EncodedType.String }

                    let! writeLine =
                        MemberRef.AddMethod
                            { Class = MemberRefParent.TypeRef console
                              MemberName = Identifier.ofStr "WriteLine"
                              Signature =
                                { HasThis = false
                                  ExplicitThis = false
                                  ReturnType = ReturnTypeItem.Void
                                  Parameters = ImmutableArray.Create string
                                  VarArgParameters = ImmutableArray.Empty } }
                    let! tfmAttrCtor =
                        MemberRef.AddMethod
                            { Class = MemberRefParent.TypeRef tfmAttr
                              MemberName = Identifier.ofStr ".ctor"
                              Signature =
                                { HasThis = true // TODO: Figure out flags.
                                  ExplicitThis = false
                                  ReturnType = ReturnTypeItem.Void
                                  Parameters = ImmutableArray.Create string
                                  VarArgParameters = ImmutableArray.Empty } }

                    CustomAttribute.Add
                        { Parent = CustomAttributeParent.Assembly assm
                          Type = CustomAttributeType.MemberRef tfmAttrCtor
                          Value =
                            { FixedArg = FixedArg.Elem (SerString ".NETCoreApp,Version=v5.0") |> ImmutableArray.Create
                              NamedArg = ImmutableArray.Empty (* FrameworkDisplayName = "" *) }
                            |> Some }

                    let! methodList =
                        methods {
                            StaticClassMethod.Method
                                { Body =
                                    [|
                                        Ldstr "Hello World!"
                                        Call (Callee.MethodRef writeLine)
                                        Ret
                                    |]
                                    |> ImmutableArray.Create<Opcode>
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
                    let! program =
                        TypeDef.AddClass
                            { Access = TypeVisibility.Public
                              ClassName = Identifier.ofStr "Program"
                              Extends = Extends.TypeRef object
                              Fields = FieldList.Empty
                              Flags = StaticClassFlags ClassFlags.Zero
                              TypeNamespace = "HelloWorld"
                              Methods = methodList }
                    entrypoint
                        (fun _ -> true)
                        program
                }
                |> ValidationResult.get
            let pe = PEFile.ofMetadata IsExe tables |> WritePE.toArray

            use reader = new PEReader(ImmutableArray.Create<byte> pe)
            let metadata = reader.GetMetadataReader()

            Expect.isNonEmpty metadata.TypeDefinitions "assembly should contain types"
            Expect.isNonEmpty metadata.MethodDefinitions "assembly should contain methods"
            Expect.equal
                (metadata.GetModuleDefinition().Mvid |> metadata.GetGuid)
                mvid
                "modules should contain same mvid"
            let assemblyattrs = metadata.GetAssemblyDefinition().GetCustomAttributes()
            Expect.isNonEmpty assemblyattrs "assembly should contain at least one custom attribute"
            Expect.all
                assemblyattrs
                (fun handle -> metadata.GetCustomAttribute(handle).Parent.Kind = HandleKind.AssemblyDefinition)
                "attributes should be parented to the assembly"
    ]
