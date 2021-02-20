module FSharpIL.Benchmarks

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open System
open System.Collections.Immutable
open System.IO

open Mono.Cecil
open Mono.Cecil.Cil

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.PortableExecutable

[<StatisticalTestColumn>]
[<MemoryDiagnoser>]
type HelloWorld () =
    [<Benchmark>]
    member _.FSharpIL_ComputationExpression() =
        metadata {
            let! assm =
                setAssembly
                    { Name = AssemblyName.ofStr "HelloWorld"
                      HashAlgId = ()
                      Version = Version()
                      Flags = ()
                      PublicKey = None
                      Culture = NullCulture }
            let! mscorlib =
                referenceAssembly
                    { Version = Version(5, 0, 0, 0)
                      PublicKeyOrToken = PublicKeyToken(0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
                      Name = AssemblyName.ofStr "System.Private.CoreLib"
                      Culture = NullCulture
                      HashValue = None }
            let! consolelib =
                referenceAssembly
                    { Version = Version(5, 0, 0, 0)
                      PublicKeyOrToken = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
                      Name = AssemblyName.ofStr "System.Console"
                      Culture = NullCulture
                      HashValue = None }

            let! console =
                referenceType
                    { TypeName = Identifier.ofStr "Console"
                      TypeNamespace = "System"
                      ResolutionScope = ResolutionScope.AssemblyRef consolelib }
            let! object =
                referenceType
                    { TypeName = Identifier.ofStr "Object"
                      TypeNamespace = "System"
                      ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
            let! tfmAttr =
                referenceType
                    { TypeName = Identifier.ofStr "TargetFrameworkAttribute"
                      TypeNamespace = "System.Runtime.Versioning"
                      ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
            let string = paramItem ImmutableArray.Empty EncodedType.String

            let! writeLine =
                referenceMethod
                    { Class = MemberRefParent.TypeRef console
                      MemberName = Identifier.ofStr "WriteLine"
                      Signature =
                        { HasThis = false
                          ExplicitThis = false
                          ReturnType = ReturnTypeItem.Void
                          Parameters = ImmutableArray.Create string
                          VarArgParameters = ImmutableArray.Empty } }
            let! tfmAttrCtor =
                referenceMethod
                    { Class = MemberRefParent.TypeRef tfmAttr
                      MemberName = Identifier.ofStr ".ctor"
                      Signature =
                        { HasThis = true // TODO: Figure out flags.
                          ExplicitThis = false
                          ReturnType = ReturnTypeItem.Void
                          Parameters = ImmutableArray.Create string
                          VarArgParameters = ImmutableArray.Empty } }

            do!
                { Parent = CustomAttributeParent.Assembly assm
                  Type = CustomAttributeType.MemberRef tfmAttrCtor
                  Value =
                    { FixedArg = FixedArg.Elem (SerString ".NETCoreApp,Version=v5.0") |> ImmutableArray.Create
                      NamedArg = ImmutableArray.Empty (* FrameworkDisplayName = "" *) }
                    |> Some }
                |> attribute

            let main =
                { Body =
                    fun content ->
                        let writer = MethodBodyWriter content
                        writer.Ldstr "Hello World!"
                        writer.Call writeLine
                        writer.Ret()
                    |> MethodBody.create
                  ImplFlags = MethodImplFlags.None
                  MethodName = Identifier.ofStr "Main"
                  Flags = Flags.staticMethod { Visibility = Public; HideBySig = true }
                  Signature =
                    let args =
                        EncodedType.Array(EncodedType.String, ArrayShape.OneDimension)
                        |> paramItem ImmutableArray.Empty
                        |> ImmutableArray.Create
                    StaticMethodSignature(MethodCallingConventions.Default, ReturnTypeItem.Void, args)
                  ParamList = fun _ _ -> Param { Flags = ParamFlags.None; ParamName = "args" } }
                |>  StaticClassMethod.Method

            let! programBuilder =
                buildStaticClass
                    { Access = TypeVisibility.Public
                      ClassName = Identifier.ofStr "Program"
                      Extends = Extends.TypeRef object
                      Flags = Flags.staticClass ClassFlags.None
                      TypeNamespace = "HelloWorld" }

            let main' = programBuilder.Methods.Add main |> ValueOption.get
            do! setEntrypoint main'
            let! _ = programBuilder.BuildType
            ()
        }
        |> CliMetadata.createMetadata
            { Name = Identifier.ofStr "HelloWorld.dll"
              Mvid = Guid.NewGuid() }
        |> ValidationResult.get
        |> PEFile.ofMetadata IsDll
        |> WritePE.toArray

    [<Benchmark>]
    member _.Mono_Cecil() =
        use assembly =
            AssemblyDefinition.CreateAssembly(
                AssemblyNameDefinition("HelloWorld", Version()),
                "HelloWorld.dll",
                ModuleKind.Console
            )

        let mdle = assembly.MainModule

        let mscorlib = AssemblyNameDefinition("System.Private.CoreLib", Version(5, 0, 0, 0))
        mscorlib.PublicKeyToken <- [| 0x7cuy; 0xecuy; 0x85uy; 0xd7uy; 0xbeuy; 0xa7uy; 0x79uy; 0x8euy |]
        mdle.AssemblyReferences.Add mscorlib

        let consolelib = AssemblyNameDefinition("System.Console", Version(5, 0, 0, 0))
        consolelib.PublicKeyToken <- [| 0xb0uy; 0x3fuy; 0x5fuy; 0x7fuy; 0x11uy; 0xd5uy; 0x0auy; 0x3auy |]
        mdle.AssemblyReferences.Add consolelib

        let console = TypeReference("System", "Console", null, consolelib) |> mdle.ImportReference
        let tfmAttr = TypeReference("System.Runtime.Versioning", "TargetFrameworkAttribute", null, mscorlib) |> mdle.ImportReference

        let str = ParameterDefinition mdle.TypeSystem.String
        let writeLine = MethodReference("WriteLine", mdle.TypeSystem.Void, console) |> mdle.ImportReference
        writeLine.Parameters.Add str
        let tfmAttrCtor = MethodReference(".ctor", mdle.TypeSystem.Void, tfmAttr) |> mdle.ImportReference
        tfmAttrCtor.Parameters.Add str

        let program = TypeDefinition("HelloWorld", "Program", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)
        let main = MethodDefinition("Main", MethodAttributes.HideBySig ||| MethodAttributes.Public, mdle.TypeSystem.Void)

        mdle.Types.Add program
        program.BaseType <- mdle.TypeSystem.Object
        program.Methods.Add main
        ArrayType mdle.TypeSystem.String |> ParameterDefinition |> main.Parameters.Add
        assembly.EntryPoint <- main

        let body = main.Body.GetILProcessor()
        body.Emit(OpCodes.Ldstr, "Hello World!")
        body.Emit(OpCodes.Call, writeLine)
        body.Emit(OpCodes.Ret)

        let tfm = CustomAttribute tfmAttrCtor
        CustomAttributeArgument(mdle.TypeSystem.String, ".NETCoreApp,Version=v5.0") |> tfm.ConstructorArguments.Add
        assembly.CustomAttributes.Add tfm

        assembly.Write(Stream.Null)

[<EntryPoint>]
let main argv =
    let assm = System.Reflection.Assembly.GetExecutingAssembly()
    BenchmarkSwitcher
        .FromAssembly(assm)
        .Run(args = argv)
    |> ignore
    0
