﻿(*** hide ***)
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

The following example creates a file containing a simple .NET 5 console application. The corresponding F# code that would produce
the same or similar CLI metadata as the example is shown in single line comments.

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

    (* Add references to other assemblies. *)
    let mscorlib =
        (* Contains core types such as System.Object or System.Int32 *)
        { AssemblyReference.Version = AssemblyVersion(5us, 0us, 0us, 0us)
          PublicKeyOrToken = PublicKeyToken(0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
          Name = FileName.ofStr "System.Private.CoreLib"
          Culture = ValueNone
          HashValue = ImmutableArray.Empty }

    builder.ReferenceAssembly mscorlib

    let consolelib =
        (* Contains the System.Console type *)
        { AssemblyReference.Version = AssemblyVersion(5us, 0us, 0us, 0us)
          PublicKeyOrToken = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
          Name = FileName.ofStr "System.Console"
          Culture = ValueNone
          HashValue = ImmutableArray.Empty }

    builder.ReferenceAssembly consolelib

    validated {
        (* Add references to types defined in referenced assemblies. *)
        let object =
            ReferencedType.ConcreteClass (
                resolutionScope = TypeReferenceParent.Assembly mscorlib,
                typeNamespace = ValueSome(Identifier.ofStr "System"),
                typeName = Identifier.ofStr "Object"
            )

        let! _ = builder.ReferenceType object

        (* Create the class that will contain the entrypoint method. *)
        // [<AbstractClass; Sealed>] type public Program
        let program =
            DefinedType.StaticClass (
                visibility = TypeVisibility.Public,
                flags = TypeAttributes.None,
                typeNamespace = ValueSome(Identifier.ofStr "HelloWorld"),
                enclosingClass = ValueNone,
                typeName = Identifier.ofStr "Program",
                extends = ClassExtends.ConcreteRef object
            )

        let! members = builder.DefineType program

        (* Create the body of the entrypoint method *)
        let body =
            { new DefinedMethodBody() with
                override _.WriteInstructions wr =
                    // TODO: Maybe make other way of generating method bodies, since methods that mutate ModuleBuilder in here might not work correctly.
                    ldstr &wr (builder.UserStrings.AddFolded "Hello World!")
                    pop &wr // call // TODO: Have a call here for WriteLine method.
                    ret &wr
                    wr.EstimatedMaxStack }

        (* Create the entrypoint method of the current assembly. *)
        // static member public Main: unit -> unit
        let! main =
            let def =
                DefinedMethod.EntryPoint (
                    visibility = MemberVisibility.Public,
                    flags = MethodAttributes.HideBySig,
                    name = MethodName.ofStr "Main",
                    kind = EntryPointKind.VoidNoArgs
                )
            members.AddEntryPoint(def, body)

        // TODO: Add TFM attribute

        // TODO: Set entry point token

        ()
    }
    |> ValidationResult.get

    let text (section: SectionBuilder) (directories: DataDirectoriesBuilder) =
        directories.CliHeader <- section.AddData builder
        struct(SectionName.text, SectionCharacteristics.text)

    BuildPE.create
        { DefaultHeaders.coffHeader with Characteristics = ImageFileFlags.exe }
        DefaultHeaders.optionalHeader
        (ImmutableArray.Create text)

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

        testCaseExec example' "runs correctly" __SOURCE_DIRECTORY__ "exout" "HelloWorld.dll" <| fun dotnet ->
            let out = dotnet.StandardOutput.ReadLine()
            dotnet.StandardError.ReadToEnd() |> stderr.Write

            fun() ->
                let code = dotnet.ExitCode
                test <@ code = 0 && out = "Hello World!" @>
    ]
#endif
