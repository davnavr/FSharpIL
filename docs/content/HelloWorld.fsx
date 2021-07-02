(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.HelloWorld
#if !BENCHMARK
open Expecto

open Swensen.Unquote

open Mono.Cecil
#endif
#endif
(**
# Hello World

The following example creates a file containing a simple .NET 5 console application. The corresponding F# signature code that
would produce the same or similar CLI metadata as the example is shown in single line comments.

## Example
*)
open System.Collections.Immutable

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.PortableExecutable

open FSharpIL.Writing
open FSharpIL.Writing.Cil

let example() =
    let builder =
        ModuleBuilder (
            name = Identifier.ofStr "HelloWorld.exe",
            assembly =
                { AssemblyDefinition.Version = AssemblyVersion(1us, 0us, 0us, 0us)
                  PublicKey = ImmutableArray.Empty
                  Name = FileName.ofStr "HelloWorld"
                  Culture = ValueNone }
        )

    (* Add references to other assemblies *)
    let mscorlib =
        (* Contains core types such as System.Object or System.Int32, usually System.Runtime is referenced instead *)
        { AssemblyReference.Version = AssemblyVersion(5us, 0us, 0us, 0us)
          PublicKeyOrToken = PublicKeyToken(0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
          Name = FileName.ofStr "System.Private.CoreLib"
          Culture = ValueNone
          HashValue = ImmutableArray.Empty }

    builder.ReferenceAssembly mscorlib

    let consolelib =
        (* Contains the System.Console type, which is needed to print text onto the screen *)
        { AssemblyReference.Version = AssemblyVersion(5us, 0us, 0us, 0us)
          PublicKeyOrToken = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
          Name = FileName.ofStr "System.Console"
          Culture = ValueNone
          HashValue = ImmutableArray.Empty }

    builder.ReferenceAssembly consolelib

    validated {
        let system = ValueSome(Identifier.ofStr "System")

        (* Add references to types defined in referenced assemblies *)
        let object =
            // type Object
            ReferencedType.ConcreteClass (
                resolutionScope = TypeReferenceParent.Assembly mscorlib,
                typeNamespace = system,
                typeName = Identifier.ofStr "Object"
            )

        let tfmattr =
            ReferencedType.SealedClass (
                resolutionScope = TypeReferenceParent.Assembly mscorlib,
                typeNamespace = ValueSome(Identifier.ofStr "System.Runtime.Versioning"),
                typeName = Identifier.ofStr "TargetFrameworkAttribute"
            )

        let console =
            // [<AbstractClass; Sealed>] type Console
            ReferencedType.StaticClass (
                resolutionScope = TypeReferenceParent.Assembly consolelib,
                typeNamespace = system,
                typeName = Identifier.ofStr "Console"
            )

        let! _ = builder.ReferenceType object
        let! tfmattr' = builder.ReferenceType tfmattr
        let! console' = builder.ReferenceType console

        (* Add reference to methods defined in a referenced types *)
        let stringArg = ParamItem.Param(ImmutableArray.Empty, EncodedType.String)

        // member new: string -> System.Runtime.Versioning.TargetFrameworkAttribute
        let tfmctor =
            ReferencedMethod.Constructor (
                visibility = ExternalVisibility.Public,
                parameterTypes = ImmutableArray.Create stringArg
            )

        let! _ = tfmattr'.ReferenceMethod tfmctor

        // static member WriteLine: string -> unit
        let! writeln =
            ReferencedMethod.Static (
                visibility = ExternalVisibility.Public,
                returnType = ReturnType.RVoid,
                name = MethodName.ofStr "WriteLine",
                parameterTypes = ImmutableArray.Create stringArg
            )
            |> console'.ReferenceMethod

        (* Create the class that will contain the entrypoint method *)
        // [<AbstractClass; Sealed>] type public Program
        let program =
            DefinedType.StaticClass (
                visibility = TypeVisibility.Public,
                flags = TypeAttributes.None,
                typeNamespace = ValueSome(Identifier.ofStr "HelloWorld"),
                enclosingClass = ValueNone,
                typeName = Identifier.ofStr "Program",
                extends = ClassExtends.ConcreteRef object,
                genericParameters = GenericParamList.empty
            )

        let! struct(members, _) = builder.DefineType program

        (* Create the body of the entrypoint method *)
        let body =
            { new DefinedMethodBody() with
                override _.WriteInstructions(wr, methods, _) =
                    ldstr &wr (builder.UserStrings.AddFolded "Hello World!")
                    call &wr writeln methods
                    ret &wr
                    wr.EstimatedMaxStack }

        (* Create the entrypoint method of the current assembly *)
        // static member public Main: unit -> unit
        let! _ =
            let def =
                DefinedMethod.EntryPoint (
                    visibility = MemberVisibility.Public,
                    flags = MethodAttributes.HideBySig,
                    name = MethodName.ofStr "Main",
                    kind = EntryPointKind.VoidNoArgs
                )
            members.AddEntryPoint(def, body)

        (* Sets the target framework of the assembly, this is so the CoreCLR and tools such as ILSpy can recognize it *)
        // [<assembly: System.Runtime.Versioning.TargetFrameworkAttribute(".NETCoreApp,Version=v5.0")>]
        do!
            builder.AssemblyCustomAttributes.Value.Add
                { Constructor = CustomAttributeCtor.Ref(tfmattr, tfmctor)
                  FixedArguments = fun _ _ _ -> Ok(FixedArg.Elem (Elem.SerString ".NETCoreApp,Version=v5.0"))
                  NamedArguments = ImmutableArray.Empty }
    }
    |> ValidationResult.get

    BuildPE.exeFromModule builder

(*** hide ***)
#if COMPILED && !BENCHMARK
[<Tests>]
let tests =
    let example' = lazy example()
    let metadata = lazy PEFile.toCecilModule example'.Value

    afterRunTests <| fun() -> if metadata.IsValueCreated then metadata.Value.Dispose()

    testList "hello world" [
        testCase "has entrypoint with correct name and signature" <| fun() ->
            let epoint = metadata.Value.EntryPoint
            <@
                epoint <> null
                && epoint.Name = "Main"
                && epoint.Parameters.Count = 0
                && epoint.ReturnType = metadata.Value.TypeSystem.Void
            @>
            |> test

        testCase "has method references only" <| fun() ->
            test <@ metadata.Value.GetMemberReferences() |> Seq.forall (fun mref -> mref :? MethodReference) @>

        testCase "has target framework attribute" <| fun() ->
            let attribute = Seq.head metadata.Value.Assembly.CustomAttributes
            let value = (Seq.head attribute.ConstructorArguments).Value
            test <@ attribute.AttributeType.Name = "TargetFrameworkAttribute" && ".NETCoreApp,Version=v5.0".Equals value @>

        testCase "has types with correct names" <| fun() ->
            let expected =
                [
                    ("", "<Module>")
                    ("HelloWorld", "Program")
                ]
            let actual =
                metadata.Value.Types
                |> Seq.map (fun tdef -> tdef.Namespace, tdef.Name)
                |> List.ofSeq
            test <@ expected = actual @>

        testCaseExec example' "prints correct message" __SOURCE_DIRECTORY__ "exout" "HelloWorld.dll" <| fun dotnet ->
            let out = dotnet.StandardOutput.ReadLine()
            dotnet.StandardError.ReadToEnd() |> stderr.Write

            fun() ->
                let code = dotnet.ExitCode
                test <@ code = 0 && out = "Hello World!" @>
    ]
#endif
