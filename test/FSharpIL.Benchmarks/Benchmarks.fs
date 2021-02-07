module FSharpIL.Benchmarks

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

[<StatisticalTestColumn>]
[<MemoryDiagnoser>]
[<SimpleJob(targetCount = 30)>]
type HelloWorld () =
    [<Benchmark>]
    member _.FSharpIL() =
        // TODO: Figure out how to share code with unit test.
        let mdle =
            { Name = Identifier.ofStr "HelloWorld.dll"
              Mvid = Guid.NewGuid() }
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
        PEFile.ofMetadata IsDll tables |> WritePE.toArray

[<EntryPoint>]
let main argv =
    let assm = System.Reflection.Assembly.GetExecutingAssembly()
    BenchmarkSwitcher
        .FromAssembly(assm)
        .Run(args = argv)
    |> ignore
    0
