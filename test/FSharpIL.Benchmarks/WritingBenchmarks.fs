module FSharpIL.WritingBenchmarks

open System

open BenchmarkDotNet.Attributes

open Mono.Cecil
open Mono.Cecil.Cil

let helloWorldCecil() =
    let assembly =
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

    assembly

[<MemoryDiagnoser>]
type ToArrayBenchmarks () =
    [<Benchmark(Baseline = true)>]
    member _.HelloWorld() = HelloWorld.example() |> FSharpIL.Writing.WritePE.block
    [<Benchmark>]
    member _.Mono_Cecil() =
        use bytes = new System.IO.MemoryStream()
        use assembly = helloWorldCecil()
        assembly.Write bytes
        bytes.GetBuffer()

[<MemoryDiagnoser>]
type ToNullStreamBenchmarks () =
    [<Benchmark(Baseline = true)>]
    member _.HelloWorld() = HelloWorld.example() |> FSharpIL.Writing.WritePE.toStream System.IO.Stream.Null
    [<Benchmark>]
    member _.Mono_Cecil() =
        use assembly = helloWorldCecil()
        assembly.Write System.IO.Stream.Null
