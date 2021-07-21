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
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

open FSharpIL.Cli

open FSharpIL.Writing
open FSharpIL.Writing.Cil

let example() = // TODO: Make helper function to add reference to System.Private.CoreLib and System.Console for example scripts.
    let namespace' = Identifier.ofStr "GenericsShowcase"

    let builder =
        CliModuleBuilder (
            name = namespace' + ".exe",
            assembly =
                { DefinedAssembly.Version = AssemblyVersion(4us, 3us, 2us, 1us)
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
        { ReferencedAssembly.Version = AssemblyVersion(5us, 0us, 0us, 0us)
          PublicKeyOrToken = NoPublicKeyOrToken
          Name = FileName.ofStr "System.Console"
          Culture = ValueNone
          HashValue = ImmutableArray.Empty }

    builder.ReferenceAssembly consolelib

    validated {
        let! mscorlib' = mscorlib.AddReferencesTo builder

        (* Generates a generic type definition. *)
        // type [<Sealed>] ArrayList<'T> = inherit System.Object
        let arrlist =
            let object' =
                mscorlib.Object.Reference
                |> ReferencedType.Reference
                |> NamedType.ReferencedType
                |> ClassExtends.Named

            let parameters = GenericParam(Identifier.ofStr "T") |> ImmutableArray.Create

            (*
                Notice how the name of the type at the IL level includes the number of generic parameters, following CLS rule 43
                (I.10.7.2)
            *)
            TypeDefinition.SealedClass (
                visibility = TypeVisibility.Public,
                flags = TypeAttributes.BeforeFieldInit,
                typeNamespace = ValueSome namespace',
                enclosingClass = ValueNone,
                typeName = Identifier.ofStr "ArrayList`1",
                extends = object'
            )
            |> GenericType.definedKind parameters

        let! members = builder.DefineGenericType(arrlist, attributes = ValueNone)

        // val private items: 'T[]
        let! items =
            let definition =
                DefinedField.Instance (
                    MemberVisibility.Private,
                    flags = FieldAttributes.InitOnly,
                    name = Identifier.ofStr "items",
                    signature = CliType.TypeVar arrlist.Parameters.[0]
                )

            members.Members.DefineField(definition, attributes = ValueNone)

        // new: capacity: int32 -> ArrayList<'T>

        // static member Main: unit -> unit
        let! _ =
            let main =
                DefinedMethod.EntryPoint (
                    MemberVisibility.Public,
                    MethodAttributes.None,
                    MethodName.ofStr "Main",
                    EntryPointKind.VoidNoArgs
                )
            let body =
                { new DefinedMethodBody() with
                    override _.WriteInstructions(wr, tokens) =
                        ret &wr
                        wr.EstimatedMaxStack }
            builder.GlobalMembers.DefineEntryPoint(main, body, attributes = ValueNone)

        do! // [<assembly: System.Runtime.Versioning.TargetFrameworkAttribute(".NETCoreApp,Version=v5.0")>]
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
    testList "generics showcase" [
        testCaseExec (lazy example()) "name of test case" __SOURCE_DIRECTORY__ "exout" "GenericsShowcase.dll" <| fun dotnet ->
            let out = dotnet.StandardOutput.ReadLine()
            dotnet.StandardError.ReadToEnd() |> stderr.Write

            fun() -> test <@ failwith "TODO: Check stdout" @>
    ]
#endif
