(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.HelloWorld
#if !BENCHMARK
open Expecto

open Swensen.Unquote
open System.IO

open Mono.Cecil

open FSharpIL
#endif
#endif
(**
# Hello World

The following example creates a simple .NET 5 console application.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

let example() =
    let builder =
        { Name = Identifier.ofStr "HelloWorld.exe"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    (* Define information for the current assembly. *)
    let assem =
        { Name = AssemblyName.ofStr "HelloWorld"
          HashAlgId = ()
          Version = Version()
          Flags = ()
          PublicKey = None
          Culture = NullCulture }
        |> Assembly.setRow builder

    (* This computation keeps track of warnings, CLS violations, and errors *)
    validated {
        (* Add references to other assemblies. *)
        let! mscorlib =
            let token =
                PublicKeyToken(0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
                |> builder.Blobs.MiscBytes.GetOrAdd
                |> PublicKeyOrToken
            AssemblyRef (
                Version(5, 0, 0, 0),
                AssemblyName.ofStr "System.Private.CoreLib",
                token
            )
            |> AssemblyRef.addRowChecked builder

        let! consolelib =
            let token =
                PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
                |> builder.Blobs.MiscBytes.GetOrAdd
                |> PublicKeyOrToken
            AssemblyRef (
                Version(5, 0, 0, 0),
                AssemblyName.ofStr "System.Console",
                token
            )
            |> AssemblyRef.addRowChecked builder

        (* Add references to types defined in referenced assemblies. *)
        let! console = TypeRef.tryCreateReflectedRow builder (ResolutionScope.AssemblyRef consolelib) typeof<Console>
        let! object = TypeRef.tryCreateReflectedRow builder (ResolutionScope.AssemblyRef mscorlib) typeof<Object>
        let! tfmattr =
            TypeRef (
                ResolutionScope.AssemblyRef mscorlib,
                Identifier.ofStr "TargetFrameworkAttribute",
                "System.Runtime.Versioning"
            )
            |> TypeRef.tryAddRowChecked builder

        (* Add references to methods defined in the types of referenced assemblies. *)
        let string = ParamItem.create EncodedType.String

        // static member WriteLine(_: string): System.Void
        let! writeLine =
            let signature = MethodRefDefaultSignature(ReturnType.itemVoid, ImmutableArray.Create string)
            { Class = MemberRefParent.TypeRef console
              MemberName = Identifier.ofStr "WriteLine"
              Signature = builder.Blobs.MethodRefSig.GetOrAdd signature }
            |> MethodRef.addRowDefaultChecked builder
        // new(_: string)
        let! tfmctor =
            let signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid, ImmutableArray.Create string)
            { Class = MemberRefParent.TypeRef tfmattr
              MemberName = Identifier.ofStr ".ctor"
              Signature = builder.Blobs.MethodRefSig.GetOrAdd signature }
            |> MethodRef.addRowDefaultChecked builder

        (* Defines a custom attribute on the current assembly specifying the target framework. *)
        // [<assembly: System.Runtime.Versioning.TargetFrameworkAttribute(".NETCoreApp,Version=v5.0")>]
        { FixedArg = FixedArg.Elem (SerString ".NETCoreApp,Version=v5.0") |> ImmutableArray.Create
          NamedArg = ImmutableArray.Empty }
        |> builder.Blobs.CustomAttribute.GetOrAdd
        |> ValueSome
        |> CustomAttribute.createRow
            builder
            (CustomAttributeParent.Assembly assem)
            (CustomAttributeType.MethodRefDefault tfmctor)

        (* Create the class that will contain the entrypoint method. *)
        // [<AbstractClass; Sealed>] type Program
        let! program =
            { Access = TypeVisibility.Public
              ClassName = Identifier.ofStr "Program"
              Extends = Extends.TypeRef object
              Flags = Flags.staticClass(ClassFlags(AutoLayout, AnsiClass))
              TypeNamespace = "HelloWorld" }
            |> StaticClass.tryAddRow builder

        (* Create the entrypoint method of the current assembly. *)
        // [<EntryPoint>] static member Main(args: string[]): System.Void
        let! main =
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
            |> EntryPoint.tryAddRow builder (StaticClass.typeIndex program)

        EntryPoint.set builder main

        return CliMetadata builder
    }
    |> ValidationResult.get
    |> PEFile.ofMetadata ImageFileFlags.exe
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
