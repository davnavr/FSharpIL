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
        invalidOp "// TODO: Figure out how to share this code with the HelloWorld.fsx example file."
        |> PEFile.ofMetadata ImageFileFlags.exe
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
