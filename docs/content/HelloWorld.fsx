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
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Blobs
open FSharpIL.Metadata.Tables
open FSharpIL.PortableExecutable

open FSharpIL.Writing
open FSharpIL.Writing.Abstractions
open FSharpIL.Writing.Tables

let example() =
    let struct(builder, assembly) =
        ModuleBuilder.createAssembly
            (fun strings guid _ ->
                (* Define information about the current module, see (II.6). *)
                let name = strings.Add(Identifier.ofStr "HelloWorld.exe")
                let mvid = guid.AddNew()
                ModuleRow.create name mvid)
            (fun strings _ blob ->
                (* Define information about the current assembly.
                   For the relationship between assemblies and modules, see (II.6.1). *)
                { Name = strings.Add(FileName.ofStr "HelloWorld")
                  HashAlgId = AssemblyHashAlgorithm.None
                  MajorVersion = 1us
                  MinorVersion = 0us
                  BuildNumber = 0us
                  RevisionNumber = 0us
                  Flags = AssemblyFlags.None
                  Culture = strings.EmptyString
                  PublicKey = blob.EmptyBlob })
            CliHeader.defaultFields
            CliMetadataRoot.defaultFields

    (* Add references to other assemblies. *)
    let mscorlib =
        // TODO: Create special PublicKeyToken type that is only 8 bytes long.
        let token = builder.Blob.Add [| 0x7cuy; 0xecuy; 0x85uy; 0xd7uy; 0xbeuy; 0xa7uy; 0x79uy; 0x8euy |]

        (* Contains core types such as System.Object or System.Int32 *)
        ModuleBuilder.referenceAssembly
            (Version(5, 0, 0, 0))
            (PublicKeyOrTokenOffset.Token token)
            (FileName.ofStr "System.Private.CoreLib")
            ValueNone
            builder.Blob.EmptyBlob
            builder

    let consolelib =
        let token = builder.Blob.Add [| 0xb0uy; 0x3fuy; 0x5fuy; 0x7fuy; 0x11uy; 0xd5uy; 0x0auy; 0x3auy |]

        (* Contains the System.Console type *)
        ModuleBuilder.referenceAssembly
            (Version(5, 0, 0, 0))
            (PublicKeyOrTokenOffset.Token token)
            (FileName.ofStr "System.Console")
            ValueNone
            builder.Blob.EmptyBlob
            builder

    validated {
        

        return failwith "TODO: Add hello world things"
    }

    let text (section: SectionBuilder) (directories: DataDirectoriesBuilder) =
        directories.CliHeader <- section.AddData(failwith "TODO: Add CLI metadata": CliMetadataBuilder)
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
        testCase "has entrypoint" <| fun() ->
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

        testCaseExec example' "runs correctly" __SOURCE_DIRECTORY__ "exout" "HelloWorld.dll" <| fun dotnet ->
            let out = dotnet.StandardOutput.ReadLine()
            dotnet.StandardError.ReadToEnd() |> stderr.Write

            fun() ->
                let code = dotnet.ExitCode
                test <@ code = 0 && out = "Hello World!" @>
    ]
#endif
