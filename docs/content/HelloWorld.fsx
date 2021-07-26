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

open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures
open FSharpIL.Metadata.Tables

open FSharpIL.Cli

open FSharpIL.Writing
open FSharpIL.Writing.Cil

let example() =
    let builder =
        CliModuleBuilder (
            name = Identifier.ofStr "HelloWorld.exe",
            assembly =
                { DefinedAssembly.Version = AssemblyVersion(1us, 0us, 0us, 0us)
                  PublicKey = ImmutableArray.Empty
                  Name = FileName.ofStr "HelloWorld"
                  Culture = ValueNone }
        )

    (* Add references to other assemblies *)
    let mscorlib =
        (* Contains core types such as System.Object or System.Int32, usually System.Runtime is referenced instead *)
        { ReferencedAssembly.Version = AssemblyVersion(5us, 0us, 0us, 0us)
          PublicKeyOrToken = PublicKeyToken(0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
          Name = FileName.ofStr "System.Private.CoreLib"
          Culture = ValueNone
          HashValue = ImmutableArray.Empty }

    builder.ReferenceAssembly mscorlib

    let consolelib =
        (* Contains the System.Console type, which is needed to print text onto the screen *)
        { ReferencedAssembly.Version = AssemblyVersion(5us, 0us, 0us, 0us)
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
            TypeReference.ConcreteClass (
                resolutionScope = TypeReferenceParent.Assembly mscorlib,
                typeNamespace = system,
                typeName = Identifier.ofStr "Object"
            )

        let tfmattr =
            // [<Sealed>] type TargetFrameworkAttribute
            TypeReference.SealedClass (
                resolutionScope = TypeReferenceParent.Assembly mscorlib,
                typeNamespace = ValueSome(Identifier.ofStr "System.Runtime.Versioning"),
                typeName = Identifier.ofStr "TargetFrameworkAttribute"
            )

        let console =
            // [<AbstractClass; Sealed>] type Console
            TypeReference.StaticClass (
                resolutionScope = TypeReferenceParent.Assembly consolelib,
                typeNamespace = system,
                typeName = Identifier.ofStr "Console"
            )

        let! _ = builder.ReferenceType object
        let! tfmattr' = builder.ReferenceType tfmattr
        let! console' = builder.ReferenceType console

        (* Add references to methods defined in a referenced types *)

        // new: string -> System.Runtime.Versioning.TargetFrameworkAttribute
        let tfmctor =
            ReferencedMethod.Constructor (
                visibility = ExternalVisibility.Public,
                parameterTypes = ImmutableArray.Create(ParameterType.T PrimitiveType.String)
            )

        let! tfmctor' = tfmattr'.ReferenceMethod tfmctor

        // static member WriteLine: string -> unit
        let! writeln =
            ReferencedMethod.Static (
                visibility = ExternalVisibility.Public,
                returnType = ReturnType.Void',
                name = MethodName.ofStr "WriteLine",
                parameterTypes = ImmutableArray.Create(ParameterType.T PrimitiveType.String)
            )
            |> console'.ReferenceMethod

        (* Create the class that will contain the entrypoint method *)
        // [<AbstractClass; Sealed>] type public Program = inherit System.Object
        let program =
            let object' =
                object.Reference
                |> ReferencedType.Reference
                |> NamedType.ReferencedType
                |> ClassExtends.Named

            TypeDefinition.StaticClass (
                visibility = TypeVisibility.Public,
                flags = TypeAttributes.None,
                typeNamespace = ValueSome(Identifier.ofStr "HelloWorld"),
                enclosingClass = ValueNone,
                typeName = Identifier.ofStr "Program",
                extends = object'
            )

        let! members = builder.DefineType(program, ValueNone)

        (* Create the entrypoint method of the current assembly *)
        // static member public Main: unit -> unit
        let! _ =
            let main =
                DefinedMethod.EntryPoint (
                    visibility = MemberVisibility.Public,
                    flags = MethodAttributes.HideBySig,
                    name = MethodName.ofStr "Main",
                    kind = EntryPointKind.VoidNoArgs
                )

            let body = MethodBody.ofSeq [
                // System.Console.WriteLine "Hello World!"
                ldstr "Hello World!"
                call writeln.Token
                ret
            ]

            members.Members.DefineEntryPoint(main, body, attributes = ValueNone)

        (* Sets the target framework of the assembly, this is so the CoreCLR and tools such as ILSpy can recognize it *)
        // [<assembly: System.Runtime.Versioning.TargetFrameworkAttribute(".NETCoreApp,Version=v5.0")>]
        do!
            builder.AssemblyCustomAttributes.Value.Add
                { Constructor = CustomAttributeCtor.Referenced tfmctor'
                  FixedArguments = fun _ _ _ -> Ok(FixedArg.Elem (Elem.SerString ".NETCoreApp,Version=v5.0"))
                  NamedArguments = ImmutableArray.Empty }
    }
    |> ValidationResult.get

    BuildPE.ofModuleBuilder FSharpIL.PortableExecutable.FileCharacteristics.IsExe builder

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
