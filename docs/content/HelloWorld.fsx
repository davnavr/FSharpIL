(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.HelloWorld

open Expecto

open Swensen.Unquote

open System.Diagnostics
open System.IO

open Newtonsoft.Json
open Newtonsoft.Json.Linq

open Mono.Cecil

open FSharpIL
#endif
(**
# Hello World

The following example creates a simple .NET 5 console application.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.PortableExecutable

let example() =
    metadata {
        // Define information for the current assembly.
        let! assm =
            setAssembly
                { Name = AssemblyName.ofStr "HelloWorld"
                  HashAlgId = ()
                  Version = Version()
                  Flags = ()
                  PublicKey = None
                  Culture = NullCulture }

        // Add references to other assemblies.
        let! mscorlib =
            referenceAssembly
                { Version = Version(5, 0, 0, 0)
                  PublicKeyOrToken = PublicKeyToken(0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
                  Name = AssemblyName.ofStr "System.Private.CoreLib"
                  Culture = NullCulture
                  HashValue = None }
        let! consolelib =
            referenceAssembly
                { Version = Version(5, 0, 0, 0)
                  PublicKeyOrToken = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
                  Name = AssemblyName.ofStr "System.Console"
                  Culture = NullCulture
                  HashValue = None }

        // Add references to types defined in referenced assemblies.
        let! console =
            referenceType
                { TypeName = Identifier.ofStr "Console"
                  TypeNamespace = "System"
                  ResolutionScope = ResolutionScope.AssemblyRef consolelib }
        let! object =
            referenceType
                { TypeName = Identifier.ofStr "Object"
                  TypeNamespace = "System"
                  ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        let! tfmAttr =
            referenceType
                { TypeName = Identifier.ofStr "TargetFrameworkAttribute"
                  TypeNamespace = "System.Runtime.Versioning"
                  ResolutionScope = ResolutionScope.AssemblyRef mscorlib }

        // Add references to methods defined in the types of referenced assemblies.
        let string = ParamItem.create EncodedType.String

        let! writeLine =
            referenceMethod
                { Class = MemberRefParent.TypeRef console
                  MemberName = Identifier.ofStr "WriteLine"
                  Signature =
                    { HasThis = false
                      ExplicitThis = false
                      ReturnType = ReturnType.itemVoid
                      Parameters = ImmutableArray.Create string
                      VarArgParameters = ImmutableArray.Empty } }
        let! tfmAttrCtor =
            referenceMethod
                { Class = MemberRefParent.TypeRef tfmAttr
                  MemberName = Identifier.ofStr ".ctor"
                  Signature =
                    { HasThis = true // TODO: Figure out flags.
                      ExplicitThis = false
                      ReturnType = ReturnType.itemVoid
                      Parameters = ImmutableArray.Create string
                      VarArgParameters = ImmutableArray.Empty } }

        // Defines a custom attribute on the current assembly specifying the target framework.
        do!
            { Parent = CustomAttributeParent.Assembly assm
              Type = CustomAttributeType.MemberRef tfmAttrCtor
              Value =
                { FixedArg = FixedArg.Elem (SerString ".NETCoreApp,Version=v5.0") |> ImmutableArray.Create
                  NamedArg = ImmutableArray.Empty }
                |> Some }
            |> attribute

        // Create the entrypoint method of the current assembly.
        let main =
            { Body =
                fun content ->
                    let writer = MethodBodyWriter content
                    writer.Ldstr "Hello World!"
                    writer.Call writeLine
                    writer.Ret()
                |> MethodBody.create
              ImplFlags = MethodImplFlags.None
              MethodName = Identifier.ofStr "Main"
              Flags = Flags.staticMethod { Visibility = Public; HideBySig = true }
              Signature = EntryPointSignature.voidWithArgs
              ParamList = fun _ _ -> Param { Flags = ParamFlags.None; ParamName = "args" } }
            |> EntryPointMethod

        // Create the class that will contain the entrypoint method.
        let! program =
            addStaticClass
                { Access = TypeVisibility.Public
                  ClassName = Identifier.ofStr "Program"
                  Extends = Extends.TypeRef object
                  Flags = Flags.staticClass { ClassFlags.None with BeforeFieldInit = true }
                  TypeNamespace = "HelloWorld" }

        let! main' = addMethod program main

        do! setEntryPoint main'
    }
    |> CliMetadata.createMetadata
        { Name = Identifier.ofStr "HelloWorld.exe"
          Mvid = Guid.NewGuid() }
    |> ValidationResult.get
    |> PEFile.ofMetadata ImageFileFlags.dll // TODO: This should use exe flags.
(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    let testCaseCecil name test =
        fun() ->
            use metadata = example() |> WritePE.stream |> ModuleDefinition.ReadModule
            test metadata
        |> testCase name

    testList "hello world" [
        testCaseCecil "has entrypoint" <| fun metadata ->
            test <@ metadata.EntryPoint.Name = "Main" @>

        testCaseCecil "has method references only" <| fun metadata ->
            test <@ metadata.GetMemberReferences() |> Seq.forall (fun mref -> mref :? MethodReference) @>

        testCaseCecil "has target framework attribute" <| fun metadata ->
            <@
                let attribute = Seq.head metadata.Assembly.CustomAttributes
                let value = Seq.head attribute.ConstructorArguments
                attribute.AttributeType.Name = "TargetFrameworkAttribute" && ".NETCoreApp,Version=v5.0".Equals value.Value
            @>
            |> test

        testCaseCecil "has types with correct names" <| fun metadata ->
            let expected =
                [
                    ("", "<Module>")
                    ("HelloWorld", "Program")
                ]
            let actual =
                metadata.Types
                |> Seq.map (fun tdef -> tdef.Namespace, tdef.Name)
                |> List.ofSeq
            test <@ expected = actual @>

        testCaseCecil "entrypoint has correct argument type" <| fun metadata ->
            let args = Seq.head metadata.EntryPoint.Parameters
            test <@ args.ParameterType.IsArray && args.ParameterType.GetElementType() = metadata.TypeSystem.String @>

        // TODO: Create common function for writing example to disk and then running.
        testCase "runs correctly" <| fun() ->
            let output = Path.Combine(__SOURCE_DIRECTORY__, "tmp")
            let executable = Path.Combine(output, "HelloWorld.dll")
            let config =
                JObject ([|
                    JProperty("runtimeOptions", JObject [|
                        JProperty("tfm", "net5.0")
                        JProperty("framework", JObject [|
                            JProperty("name", "Microsoft.NETCore.App")
                            JProperty("version", "5.0.0")
                        |])
                    |])
                |])

            example() |> WritePE.toPath executable

            using (new JsonTextWriter(new StreamWriter(Path.Combine(output, "HelloWorld.runtimeconfig.json")))) config.WriteTo

            use dotnet =
                ProcessStartInfo (
                    fileName = "dotnet",
                    arguments = sprintf "exec %s" executable,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false
                )
                |> Process.Start

            let out = dotnet.StandardOutput.ReadLine()
            dotnet.StandardError.ReadToEnd() |> stderr.Write
            dotnet.WaitForExit()

            let code = dotnet.ExitCode

            test <@ code = 0 && out = "Hello World!" @>
    ]
#endif
