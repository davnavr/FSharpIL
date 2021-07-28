(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.EventsShowcase

open Expecto

open Swensen.Unquote

#endif
(**
# Events Showcase

The following example showcases the generation of events.

Because F# provides additional classes and attributes used when creating events, the corresponding decompiled IL code is shown in
C# instead.

## Example
*)
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Cil
open FSharpIL.Metadata.Tables

open FSharpIL.Cli

open FSharpIL.Writing
open FSharpIL.Writing.Cil

let example() =
    let builder =
        CliModuleBuilder (
            name = Identifier.ofStr "EventsShowcase.exe",
            assembly =
                { DefinedAssembly.Version = AssemblyVersion.Zero
                  PublicKey = ImmutableArray.Empty
                  Name = FileName.ofStr "EventsShowcase"
                  Culture = ValueNone }
        )

    let mscorlib =
        CoreAssemblyReference.NetCore (
            version = AssemblyVersion(5us, 0us, 0us, 0us),
            publicKeyToken = (0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
        )

    let consolelib =
        { ReferencedAssembly.Version = AssemblyVersion(5us, 0us, 0us, 0us)
          PublicKeyOrToken = NoPublicKeyOrToken
          Name = FileName.ofStr "System.Console"
          Culture = ValueNone
          HashValue = ImmutableArray.Empty }

    builder.ReferenceAssembly consolelib

    validated {
        let! mscorlib' = mscorlib.AddReferencesTo builder

        let referenceDelegateMethod name pcount =
            let del =
                ReferencedType.Reference mscorlib.Delegate.Reference
                |> NamedType.ReferencedType
                |> CliType.Class

            let parameters = Array.init pcount (fun _ -> ParameterType.T del) |> ImmutableArray.CreateRange

            ReferencedMethod.Static(ExternalVisibility.Public, ReturnType.T del, MethodName.ofStr name, parameters)
            |> mscorlib'.Delegate.ReferenceMethod

        // public class EventArgs
        let evargs =
            TypeReference.ConcreteClass (
                resolutionScope = TypeReferenceParent.Assembly mscorlib.Reference,
                typeNamespace = mscorlib.Object.Reference.TypeNamespace,
                typeName = Identifier.ofStr "EventArgs"
            )

        // public delegate void EventHandler(object, EventArgs)
        let evhandler =
            TypeReference.SealedClass (
                resolutionScope = TypeReferenceParent.Assembly mscorlib.Reference,
                typeNamespace = mscorlib.Object.Reference.TypeNamespace,
                typeName = Identifier.ofStr "EventHandler"
            )

        let! _ = builder.ReferenceType evhandler

        // public static class Console
        let! console =
            TypeReference.StaticClass (
                resolutionScope = TypeReferenceParent.Assembly consolelib,
                typeNamespace = mscorlib.Object.Reference.TypeNamespace,
                typeName = Identifier.ofStr "Console"
            )
            |> builder.ReferenceType

        // public static void WriteLine (string)
        let! writeln =
            ReferencedMethod.Static (
                visibility = ExternalVisibility.Public,
                returnType = ReturnType.Void',
                name = MethodName.ofStr "WriteLine",
                parameterTypes = ImmutableArray.Create(ParameterType.T PrimitiveType.String)
            )
            |> console.ReferenceMethod

        // public static System.Delegate Combine (System.Delegate, System.Delegate)
        let! combine2 = referenceDelegateMethod "Combine" 2
        // public static System.Delegate Remove (System.Delegate, System.Delegate)
        let! remove = referenceDelegateMethod "Remove" 2

        // public class Button : System.Object
        let button =
            let object' =
                mscorlib.Object.Reference
                |> ReferencedType.Reference
                |> NamedType.ReferencedType
                |> ClassExtends.Named

            TypeDefinition.ConcreteClass (
                visibility = TypeVisibility.Public,
                flags = TypeAttributes.BeforeFieldInit,
                typeNamespace = ValueNone,
                enclosingClass = ValueNone,
                typeName = Identifier.ofStr "Button",
                extends = object'
            )

        let! button' = builder.DefineType(button, attributes = ValueNone)

        // public Button ()
        let! ctor =
            let definition =
                DefinedMethod.Constructor (
                    MemberVisibility.Public,
                    flags = MethodAttributes.None,
                    parameterTypes = ImmutableArray.Empty,
                    parameterList = Parameter.emptyList
                )

            // : base()
            let body = MethodBody.ofSeq [ ldarg_0; callvirt mscorlib'.ObjectConstructor.Token; ret ]

            button'.DefineMethod(definition, body, attributes = ValueNone)

        let handlerParameterTypes =
            ReferencedType.Reference evhandler.Reference
            |> NamedType.ReferencedType
            |> CliType.Class
            |> ParameterType.T
            |> ImmutableArray.Create

        let addClickHandler =
            let definition =
                DefinedMethod.Instance (
                    MemberVisibility.Public,
                    flags = MethodAttributes.SpecialName,
                    returnType = ReturnType.Void',
                    name = MethodName.ofStr "add_Clicked",
                    parameterTypes = handlerParameterTypes,
                    parameterList = Parameter.emptyList
                )

            let body = MethodBody.ofSeq [ ret ]

            definition :> DefinedMethod, ValueSome body, ValueNone

        let removeClickHandler =
            let definition =
                DefinedMethod.Instance (
                    MemberVisibility.Public,
                    flags = MethodAttributes.SpecialName,
                    returnType = ReturnType.Void',
                    name = MethodName.ofStr "remove_Clicked",
                    parameterTypes = handlerParameterTypes,
                    parameterList = Parameter.emptyList
                )

            let body = MethodBody.ofSeq [ ret ]

            definition :> DefinedMethod, ValueSome body, ValueNone

        let raiseClickEvent =
            let definition =
                let ptypes =
                    ReferencedType.Reference evargs.Reference
                    |> NamedType.ReferencedType
                    |> CliType.Class
                    |> ParameterType.T
                    |> ImmutableArray.Create

                DefinedMethod.Instance (
                    MemberVisibility.Assembly,
                    flags = MethodAttributes.SpecialName,
                    returnType = ReturnType.Void',
                    name = MethodName.ofStr "raise_Clicked",
                    parameterTypes = ptypes,
                    parameterList = Parameter.emptyList
                )

            let body = MethodBody.ofSeq [ ret ]

            definition :> DefinedMethod, ValueSome body, ValueNone

        // public event System.EventHandler Clicked;
        let! clicked =
            let etype =
                ReferencedType.Reference evhandler.Reference
                |> NamedType.ReferencedType
                |> TypeTok.Named

            button'.Members.DefineEvent (
                Identifier.ofStr "Clicked",
                etype,
                addClickHandler,
                removeClickHandler,
                ValueSome raiseClickEvent,
                List.empty,
                ValueNone
            )

        // public static void Main()
        let! _ =
            let definition =
                DefinedMethod.EntryPoint (
                    MemberVisibility.Public,
                    MethodAttributes.None,
                    MethodName.ofStr "Main",
                    EntryPointKind.VoidNoArgs
                )

            let locals =
                // Button button;
                DefinedType.Definition button.Definition
                |> NamedType.DefinedType
                |> CliType.Class
                |> CliType.toLocalType
                |> ImmutableArray.Create

            let body = MethodBody.create InitLocals ValueNone (LocalVariables.Locals locals) [
                InstructionBlock.ofList [
                    // button = Button();
                    Newobj.ofDefinedMethod ctor
                    stloc_0
                    ret
                ]
            ]

            builder.GlobalMembers.DefineEntryPoint(definition, body, attributes = ValueNone)

        do! // [assembly: System.Runtime.Versioning.TargetFrameworkAttribute(".NETCoreApp,Version=v5.0")]
            builder.SetTargetFramework (
                ".NETCoreApp,Version=v5.0",
                CustomAttributeCtor.Referenced mscorlib'.TargetFrameworkConstructor
            )
    }
    |> ValidationResult.get

    BuildPE.ofModuleBuilder FSharpIL.PortableExecutable.FileCharacteristics.IsExe builder

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    testList "events showcase" [
        testCaseExec (lazy example()) "prints message" __SOURCE_DIRECTORY__ "exout" "EventsShowcase.dll" <| fun dotnet ->
            let out =
                [
                    while not dotnet.StandardOutput.EndOfStream do
                        dotnet.StandardOutput.ReadLine()
                ]

            dotnet.StandardError.ReadToEnd() |> stderr.Write

            fun() ->
                let code = dotnet.ExitCode
                test <@ code = 0 && out = [ "I was clicked!" ] @>
    ]
#endif
