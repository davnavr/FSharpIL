(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.GenericsShowcase

open Expecto

open Swensen.Unquote

#endif
(**
# Generics Showcase

The following example showcases the generation of generic parameters on classes and methods, as well as the usage of generic
types and members from other assemblies.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.PortableExecutable

open FSharpIL.Writing
open FSharpIL.Writing.Cil

let example() = // TODO: Make helper function to add reference to System.Private.CoreLib and System.Console for example scripts.
    let namespace' = Identifier.ofStr "GenericsShowcase"

    let builder =
        ModuleBuilder (
            name = Identifier.ofStr "GenericsShowcase.exe",
            assembly =
                { AssemblyDefinition.Version = AssemblyVersion(4us, 3us, 2us, 1us)
                  PublicKey = ImmutableArray.Empty
                  Name = FileName.ofId namespace'
                  Culture = ValueNone }
        )

    let mscorlib =
        CoreAssemblyReference.NetCore (
            version = AssemblyVersion(5us, 0us, 0us, 0us),
            publicKeyToken = (0x7cuy, 0xecuy, 0x85uy, 0xd7uy, 0xbeuy, 0xa7uy, 0x79uy, 0x8euy)
        )

    let consolelib =
        { AssemblyReference.Version = AssemblyVersion(5us, 0us, 0us, 0us)
          PublicKeyOrToken = NoPublicKeyOrToken
          Name = FileName.ofStr "System.Console"
          Culture = ValueNone
          HashValue = ImmutableArray.Empty }

    builder.ReferenceAssembly consolelib

    validated {
        let! core = mscorlib.AddReferencesTo builder

        (*
            Generate a generic type definition.
            C#, VB, and F# compilers append the number of generic parameters to the class name, following CLS rule 43 (I.10.7.2)
        *)
        // type [<Sealed>] ArrayList<'T> = inherit System.Object
        let arrlist =
            DefinedType.SealedClass (
                visibility = TypeVisibility.Public,
                flags = TypeAttributes.BeforeFieldInit,
                typeNamespace = ValueSome namespace',
                enclosingClass = ValueNone,
                typeName = Identifier.ofStr "ArrayList`1",
                extends = ClassExtends.ConcreteRef mscorlib.Object
            )

        // TODO: Figure out how to add generic parameters.
        let! arrlist' = builder.DefineType arrlist


        let maindef =
            DefinedMethod.EntryPoint (
                MemberVisibility.Public,
                MethodAttributes.None,
                MethodName.ofStr "Main",
                EntryPointKind.VoidNoArgs
            )

        let mainbody =
            { new DefinedMethodBody() with
                override _.WriteInstructions(wr, methods) =
                    ret &wr
                    wr.EstimatedMaxStack }

        // static member Main: unit -> unit
        let! _ = builder.GlobalMembers.AddEntryPoint(maindef, mainbody)

        ()
    }
    |> ValidationResult.get

    BuildPE.exeFromModule builder

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    testList "generics showcase" [
        testCaseExec (lazy example()) "name of test case" __SOURCE_DIRECTORY__ "exout" "GenericsShowcase.dll" <| fun dotnet ->
            let out = dotnet.StandardOutput.ReadLine()
            dotnet.StandardError.ReadToEnd() |> stderr.Write

            fun() ->
                test <@ 1 = 2 @>
    ]
#endif
