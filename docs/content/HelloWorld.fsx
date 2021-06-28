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

The following example creates a file containing a simple .NET 5 console application. The corresponding F# code that would produce
the same or similar CLI metadata as the example is shown as single line comments `//`.

## Example
*)
open System.Collections.Immutable

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.PortableExecutable

open FSharpIL.Writing

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
        // [<AbstractClass; Sealed>] type Program
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

        // TODO: Add entrypoint method
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
        ftestCase "has entrypoint" <| fun() ->
            metadata.Value.EntryPoint.Name =! "Main"

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

        testCase "entrypoint has correct argument type" <| fun() ->
            let paramType = (Seq.head metadata.Value.EntryPoint.Parameters).ParameterType
            test <@ paramType.IsArray && paramType.GetElementType() = metadata.Value.TypeSystem.String @>

        ftestCaseExec example' "runs correctly" __SOURCE_DIRECTORY__ "exout" "HelloWorld.dll" <| fun dotnet ->
            let out = dotnet.StandardOutput.ReadLine()
            dotnet.StandardError.ReadToEnd() |> stderr.Write

            fun() ->
                let code = dotnet.ExitCode
                test <@ code = 0 && out = "Hello World!" @>
    ]
#endif
