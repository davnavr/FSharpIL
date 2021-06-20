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

The following example creates a file containing a simple .NET 5 console application.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables
open FSharpIL.PortableExecutable

open FSharpIL.Writing
open FSharpIL.Writing.Abstractions

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
                { Name = strings.Add(AssemblyName.ofStr "HelloWorld")
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

    failwith "TODO: Add hello world things"

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
