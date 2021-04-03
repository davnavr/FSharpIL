namespace FSharpIL

open BenchmarkDotNet.Attributes

open System
open System.Collections.Immutable

open FSharpIL
open FSharpIL.PortableExecutable

open Mono.Cecil
open Mono.Cecil.Cil

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.Metadata.Unchecked
open FSharpIL.Metadata.UncheckedExn

[<StatisticalTestColumn>]
[<MemoryDiagnoser>]
type HelloWorldBenchmarks () =
    [<Benchmark>]
    member _.FSharpIL_ComputationExpression() = HelloWorld.example()

    [<Benchmark>]
    member _.FSharpIL_UncheckedExn() =
        let builder =
            { Name = Identifier.ofStr "HelloWorld.exe"
              Mvid = Guid.NewGuid() }
            |> CliMetadataBuilder

        let assem =
            { Name = AssemblyName.ofStr "HelloWorld"
              HashAlgId = ()
              Version = Version()
              Flags = ()
              PublicKey = None
              Culture = NullCulture }
            |> setAssembly builder

        let lversion = Version(5, 0, 0, 0)

        let struct (mscorlib, _) =
            let token =
                PublicKeyToken(0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
                |> builder.Blobs.MiscBytes.GetOrAdd
                |> PublicKeyOrToken
            AssemblyRef (
                lversion,
                AssemblyName.ofStr "System.Private.CoreLib",
                token
            )
            |> referenceAssembly builder

        let struct (consolelib, _) =
            let token =
                PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
                |> builder.Blobs.MiscBytes.GetOrAdd
                |> PublicKeyOrToken
            AssemblyRef (
                lversion,
                AssemblyName.ofStr "System.Console",
                token
            )
            |> referenceAssembly builder

        let console =
            TypeRef(ResolutionScope.AssemblyRef mscorlib, Identifier.ofStr "Console", "System") |> referenceType builder
        let object =
            TypeRef(ResolutionScope.AssemblyRef mscorlib, Identifier.ofStr "Object", "System") |> referenceType builder
        let tfmAttr =
            TypeRef (
                ResolutionScope.AssemblyRef mscorlib,
                Identifier.ofStr "TargetFrameworkAttribute",
                "System.Runtime.Versioning"
            )
            |> referenceType builder
        let string = ParamItem.create EncodedType.String

        let struct (writeLine, _) =
            { Class = MemberRefParent.TypeRef console
              MemberName = Identifier.ofStr "WriteLine"
              Signature =
                MethodRefDefaultSignature(ReturnType.itemVoid, ImmutableArray.Create string)
                |> builder.Blobs.MethodRefSig.GetOrAdd }
            |> referenceDefaultMethod builder
        let struct (tfmAttrCtor, _) =
            { Class = MemberRefParent.TypeRef tfmAttr
              MemberName = Identifier.ofStr ".ctor"
              Signature =
                MethodRefDefaultSignature(true, false, ReturnType.itemVoid, ImmutableArray.Create string)
                |> builder.Blobs.MethodRefSig.GetOrAdd }
            |> referenceDefaultMethod builder

        { Parent = CustomAttributeParent.Assembly assem
          Type = CustomAttributeType.MethodRefDefault tfmAttrCtor
          Value =
            { FixedArg = FixedArg.Elem (SerString ".NETCoreApp,Version=v5.0") |> ImmutableArray.Create
              NamedArg = ImmutableArray.Empty }
            |> builder.Blobs.CustomAttribute.GetOrAdd
            |> ValueSome }
        |> addCustomAttribute builder

        let program =
            { Access = TypeVisibility.Public
              ClassName = Identifier.ofStr "Program"
              Extends = Extends.TypeRef object
              Flags = Flags.staticClass(ClassFlags(AutoLayout, AnsiClass))
              TypeNamespace = "HelloWorld" }
            |> StaticClass.addTypeDef builder

        let main =
            let body content =
                let writer = MethodBodyWriter content
                writer.Ldstr "Hello World!"
                writer.Call writeLine
                writer.Ret()
                MethodBody(maxStack = 1us)
            EntryPointMethod (
                MethodBody.create ValueNone body,
                Flags.staticMethod(StaticMethodFlags(Public, NoSpecialName, true)),
                Identifier.ofStr "Main",
                builder.Blobs.MethodDefSig.GetOrAdd EntryPointSignature.voidWithArgs,
                Param { Flags = ParamFlags(); ParamName = "args" } |> ParamList.singleton
            )
            |> StaticClass.addEntryPoint builder program

        setEntryPoint builder main

        CliMetadata builder |> PEFile.ofMetadata ImageFileFlags.exe

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

        assembly
