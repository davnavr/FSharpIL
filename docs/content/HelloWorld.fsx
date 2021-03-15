(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.HelloWorld

open Expecto

open Swensen.Unquote

open System.IO

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
open FSharpIL.Metadata.Unchecked
open FSharpIL.Metadata.Checked
open FSharpIL.Metadata.CliMetadata
open FSharpIL.PortableExecutable

let example() =
    let builder =
        { Name = Identifier.ofStr "HelloWorld.exe"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    // Define information for the current assembly.
    let assem =
        { Name = AssemblyName.ofStr "HelloWorld"
          HashAlgId = ()
          Version = Version()
          Flags = ()
          PublicKey = None
          Culture = NullCulture }
        |> setAssembly builder

    // This computation keeps track of warnings, CLS violations, and errors
    validated {
        // Add references to other assemblies.
        let! mscorlib =
            { Version = Version(5, 0, 0, 0)
              PublicKeyOrToken = PublicKeyToken(0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
              Name = AssemblyName.ofStr "System.Private.CoreLib"
              Culture = NullCulture
              HashValue = None }
            |> referenceAssembly builder

        let! consolelib =
            // Helper functions to add predefined assembly references are available in the SystemAssembly module
            SystemAssembly.Net5_0.console builder

        // Add references to types defined in referenced assemblies.
        let! console = SystemType.console builder consolelib
        let! object = SystemType.object builder mscorlib
        let! tfmAttr =
            { TypeName = Identifier.ofStr "TargetFrameworkAttribute"
              TypeNamespace = "System.Runtime.Versioning"
              ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
            |> referenceType builder

        // Add references to methods defined in the types of referenced assemblies.
        let string = ParamItem.create EncodedType.String

        let! writeLine =
            { Class = MemberRefParent.TypeRef console
              MemberName = Identifier.ofStr "WriteLine"
              Signature = MethodRefDefaultSignature(ReturnType.itemVoid, ImmutableArray.Create string) }
            |> referenceDefaultMethod builder
        let! tfmAttrCtor =
            { Class = MemberRefParent.TypeRef tfmAttr
              MemberName = Identifier.ofStr ".ctor"
              Signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid, ImmutableArray.Create string) }
            |> referenceDefaultMethod builder

        // Defines a custom attribute on the current assembly specifying the target framework.
        { Parent = CustomAttributeParent.Assembly assem
          Type = CustomAttributeType.MethodRefDefault tfmAttrCtor
          Value =
          { FixedArg = FixedArg.Elem (SerString ".NETCoreApp,Version=v5.0") |> ImmutableArray.Create
            NamedArg = ImmutableArray.Empty }
          |> Some }
        |> addCustomAttribute builder

        // Create the entrypoint method of the current assembly.
        let main =
            { Body =
                fun content ->
                    let writer = MethodBodyWriter content
                    writer.Ldstr "Hello World!"
                    writer.Call writeLine
                    writer.Ret()
                    { MaxStack = 8us; InitLocals = false }
                |> MethodBody.create
              ImplFlags = MethodImplFlags.None
              MethodName = Identifier.ofStr "Main"
              Flags = Flags.staticMethod { Visibility = Public; HideBySig = true }
              Signature = EntryPointSignature.voidWithArgs
              ParamList = fun _ _ -> Param { Flags = ParamFlags.None; ParamName = "args" } }
            |> EntryPointMethod

        // Create the class that will contain the entrypoint method.
        let! program =
            { Access = TypeVisibility.Public
              ClassName = Identifier.ofStr "Program"
              Extends = Extends.TypeRef object
              Flags = Flags.staticClass { ClassFlags.None with BeforeFieldInit = true }
              TypeNamespace = "HelloWorld" }
            |> addStaticClass builder

        let! main' = addMethod builder program main

        setEntryPoint builder main'

        return CliMetadata builder
    }
    |> ValidationResult.get
    |> PEFile.ofMetadata ImageFileFlags.exe
(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    let example' = lazy example()
    let metadata = lazy PEFile.toCecilModule example'.Value

    afterRunTests <| fun() -> metadata.Value.Dispose()

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
            <@
                expected = actual
            @>
            |> test

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
