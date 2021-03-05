(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.HelloWorld

open Expecto

open Swensen.Unquote

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
            // Helper functions to add predefined assembly references are available in the SystemAssembly module
            SystemAssembly.Net5_0.console

        // Add references to types defined in referenced assemblies.
        let! console = SystemTypes.console consolelib
        let! object = SystemTypes.object mscorlib
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
    |> PEFile.ofMetadata ImageFileFlags.exe
(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    testList "hello world" [
        testCaseCecil example "has entrypoint" <| fun metadata ->
            test <@ metadata.EntryPoint.Name = "Main" @>

        testCaseCecil example "has method references only" <| fun metadata ->
            test <@ metadata.GetMemberReferences() |> Seq.forall (fun mref -> mref :? MethodReference) @>

        testCaseCecil example "has target framework attribute" <| fun metadata ->
            <@
                let attribute = Seq.head metadata.Assembly.CustomAttributes
                let value = Seq.head attribute.ConstructorArguments
                attribute.AttributeType.Name = "TargetFrameworkAttribute" && ".NETCoreApp,Version=v5.0".Equals value.Value
            @>
            |> test

        testCaseCecil example "has types with correct names" <| fun metadata ->
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

        testCaseCecil example "entrypoint has correct argument type" <| fun metadata ->
            let args = Seq.head metadata.EntryPoint.Parameters
            test <@ args.ParameterType.IsArray && args.ParameterType.GetElementType() = metadata.TypeSystem.String @>

        testCaseExec example "runs correctly" __SOURCE_DIRECTORY__ "exout" "HelloWorld.dll" <| fun dotnet ->
            let out = dotnet.StandardOutput.ReadLine()
            dotnet.StandardError.ReadToEnd() |> stderr.Write

            fun() -> let code = dotnet.ExitCode in test <@ code = 0 && out = "Hello World!" @>
    ]
#endif
