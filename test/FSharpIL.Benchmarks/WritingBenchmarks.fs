module FSharpIL.WritingBenchmarks

open System

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Diagnosers

[<AutoOpen>]
module Cecil =
    open Mono.Cecil
    open Mono.Cecil.Cil

    let helloWorldMonoCecil() =
        let assembly =
            AssemblyDefinition.CreateAssembly(
                AssemblyNameDefinition("HelloWorld", Version(1, 0, 0, 0)),
                "HelloWorld.exe",
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
        let tfmAttr =
            TypeReference("System.Runtime.Versioning", "TargetFrameworkAttribute", null, mscorlib) |> mdle.ImportReference

        let str = ParameterDefinition mdle.TypeSystem.String
        let writeLine = MethodReference("WriteLine", mdle.TypeSystem.Void, console) |> mdle.ImportReference
        writeLine.Parameters.Add str
        let tfmAttrCtor = MethodReference(".ctor", mdle.TypeSystem.Void, tfmAttr) |> mdle.ImportReference
        tfmAttrCtor.Parameters.Add str

        let program =
            TypeDefinition("HelloWorld", "Program", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

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

[<AutoOpen>]
module AsmResolver =
    open AsmResolver.PE.DotNet.Metadata.Tables.Rows
    open AsmResolver.PE.DotNet.Cil

    open AsmResolver.DotNet
    open AsmResolver.DotNet.Code.Cil
    open AsmResolver.DotNet.Signatures
    open AsmResolver.DotNet.Signatures.Types

    let helloWorldAsmResolver() =
        let mscorlib = KnownCorLibs.SystemPrivateCoreLib_v5_0_0_0
        let manifest = ModuleDefinition("HelloWorld.exe", mscorlib)
        let assembly = new AssemblyDefinition("HelloWorld", Version(1, 0, 0, 0))

        assembly.Modules.Add manifest

        let consolelib =
            AssemblyReference (
                "System.Console",
                Version(5, 0, 0, 0),
                false,
                [| 0xb0uy; 0x3fuy; 0x5fuy; 0x7fuy; 0x11uy; 0xd5uy; 0x0auy; 0x3auy |]
            )

        manifest.AssemblyReferences.Add consolelib

        let program =
            TypeDefinition("HelloWorld", "Program", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        manifest.TopLevelTypes.Add program

        let voidsig = MethodSignature(CallingConventionAttributes.Default, manifest.CorLibTypeFactory.Void, Seq.empty)

        let main =
            MethodDefinition (
                "Main",
                MethodAttributes.Public ||| MethodAttributes.Static ||| MethodAttributes.HideBySig,
                voidsig
            )

        let importer = ReferenceImporter manifest

        let body = CilMethodBody main
        let console = importer.ImportType(TypeReference(consolelib, "System", "Console"))
        let writeln =
            let signature =
                MethodSignature(CallingConventionAttributes.Default, manifest.CorLibTypeFactory.Void, [ manifest.CorLibTypeFactory.String ])
            MemberReference(console, "WriteLine", signature) |> importer.ImportMethod

        body.Instructions.Add(CilOpCodes.Ldstr, "Hello World!") |> ignore
        body.Instructions.Add(CilOpCodes.Call, writeln) |> ignore
        body.Instructions.Add CilOpCodes.Ret |> ignore

        program.Methods.Add main
        manifest.ManagedEntrypointMethod <- main

        let tfm = TypeReference(mscorlib, "System.Runtime.Versioning", "TargetFrameworkAttribute") |> importer.ImportType
        let tfmctor = MemberReference(tfm, ".ctor", voidsig)

        tfmctor |> importer.ImportMethod |> ignore

        let tfm' =
            let tfmarg = CustomAttributeArgument(manifest.CorLibTypeFactory.String, ".NETCoreApp,Version=v5.0")
            CustomAttribute(tfmctor, CustomAttributeSignature [ tfmarg ])

        assembly.CustomAttributes.Add tfm'

        manifest

[<MemoryDiagnoser>]
type WritingToNullStream () =
    [<Benchmark(Baseline = true)>]
    member _.FSharpIL() = HelloWorld.example() |> FSharpIL.Writing.WritePE.toStream System.IO.Stream.Null

    [<Benchmark>]
    member _.Mono_Cecil() =
        use assembly = helloWorldMonoCecil()
        assembly.Write System.IO.Stream.Null

    [<Benchmark>]
    member _.AsmResolver() =
        let manifest = helloWorldAsmResolver()
        manifest.Write System.IO.Stream.Null

[<AutoOpen>]
module LargeFile =
    open System.Collections.Immutable

    open FSharpIL.Metadata
    open FSharpIL.Metadata.Tables

    open FSharpIL.Cli

    open FSharpIL.Writing

    let generateLargeFile n =
        let builder =
            CliModuleBuilder (
                name = Identifier.ofStr "LargeFile.dll",
                assembly =
                    { DefinedAssembly.Version = AssemblyVersion(1us, 0us, 0us, 0us)
                      PublicKey = ImmutableArray.Empty
                      Name = FileName.ofStr "LargeFile"
                      Culture = ValueNone }
            )

        let msignature = ImmutableArray.Create(ParameterType.T PrimitiveType.I4)

        let mutable baset = Unchecked.defaultof<_>

        for ai = 1 to n do
            let assem =
                { ReferencedAssembly.Version = AssemblyVersion(uint16 n, 0us, 0us, 0us)
                  PublicKeyOrToken = NoPublicKeyOrToken
                  Name = FileName.ofStr("AssemblyReference" + string ai)
                  Culture = ValueNone
                  HashValue = ImmutableArray.Empty }

            builder.ReferenceAssembly assem

            for ti = 1 to n do
                let tref =
                    { TypeReference.Flags = ValueNone
                      ResolutionScope = TypeReferenceParent.Assembly assem
                      TypeName = Identifier.ofStr("TypeReference" + string ai + "-" + string ti)
                      TypeNamespace = ValueNone }
                    |> ReferencedType.Reference

                if System.Object.ReferenceEquals(Unchecked.defaultof<_>, baset) then baset <- NamedType.ReferencedType tref

                let members = ValidationResult.get(builder.ReferenceType tref)

                for mi = 1 to n do
                    MethodReference.Static (
                        ExternalVisibility.Public,
                        ReturnType.Void',
                        MethodName.ofStr("MethodReference" + string mi),
                        msignature
                    )
                    |> members.ReferenceMethod 
                    |> ValidationResult.get
                    |> ignore

        for ti = 1 to n do
            let ti' = string ti
            let tdef =
                { Flags = TypeDefFlags.Public
                  Extends = ClassExtends.Named baset
                  EnclosingClass = ValueNone
                  TypeNamespace = Identifier.ofStr("Namespace" + ti') |> ValueSome
                  TypeName = Identifier.ofStr("TypeDefinition" + ti') }
                |> DefinedType.Definition

            let struct(_, members) = ValidationResult.get(builder.DefineType tdef)

            for mi = 1 to n do
                let mi' = string mi
                if mi % 2 = 0 then
                    let field =
                        FieldDefinition.Instance (
                            MemberVisibility.Public,
                            FieldAttributes.None,
                            Identifier.ofStr("Field" + mi'),
                            PrimitiveType.Object
                        )

                    members.DefineField(field, ValueNone)
                    |> ValidationResult.get
                    |> ignore
                else
                    () // Method

        BuildPE.ofModuleBuilder FSharpIL.PortableExecutable.FileCharacteristics.IsDll builder

[<EventPipeProfiler(EventPipeProfile.GcVerbose)>]
type WritingProfiled () =
    //[<Benchmark>]
    //member _.HelloWorld() = HelloWorld.example() |> FSharpIL.Writing.WritePE.toStream System.IO.Stream.Null

    [<Params(50, 100)>]
    member val N = 100 with get, set

    [<Benchmark>]
    member this.LargeCounts() = generateLargeFile this.N |> FSharpIL.Writing.WritePE.toStream System.IO.Stream.Null
