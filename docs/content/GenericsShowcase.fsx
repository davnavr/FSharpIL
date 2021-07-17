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
open FSharpIL.Metadata.Signatures

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
        { ReferencedAssembly.Version = AssemblyVersion(5us, 0us, 0us, 0us)
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
                extends = ClassExtends.ConcreteRef mscorlib.Object,
                genericParameters = GenericParamList.ofSeq [ GenericParam.named(Identifier.ofStr "T") ]
            )

        // 'T
        let t = builder.AddTypeSpec { Spec = TypeSpec.Var 0u }

        // TODO: Figure out how to add generic parameters.
        let! struct(arrlist', _) = builder.DefineType arrlist

        // val private items: 'T[]
        let! items =
            DefinedField.Instance (
                visibility = MemberVisibility.Private,
                flags = FieldAttributes.InitOnly,
                name = Identifier.ofStr "items",
                signature =
                    { CustomModifiers = ImmutableArray.Empty
                      FieldType = EncodedType.SZArray(ImmutableArray.Empty, EncodedType.Var 0u) }
            )
            |> arrlist'.AddField

        // new: capacity: int32 -> ArrayList<'T>
        let ctor =
            DefinedMethod.Constructor (
                MemberVisibility.Public,
                flags = MethodAttributes.HideBySig,
                parameterTypes = ImmutableArray.Create(ParamItem.Type(ImmutableArray.Empty, EncodedType.I4)),
                parameterList = (fun _ _ -> Parameter.named(Identifier.ofStr "capacity"))
            )

        let ctorbody =
            { new DefinedMethodBody() with
                override _.WriteInstructions(wr, methods, fields, types) =
                    // inherit System.Object()
                    ldarg_0 &wr
                    callvirt &wr core.ObjectConstructor methods

                    // { items = Array.zeroCreate<'T> capacity }
                    ldarg_0 &wr
                    ldarg_1 &wr
                    newarr &wr (TypeDefOrRefOrSpec.Spec t) types
                    stfld &wr items fields // TODO: This needs to be a memberref to the field.

                    ret &wr
                    wr.EstimatedMaxStack }

        let! ctor = arrlist'.AddMethod(ctor, ValueSome ctorbody)

        let maindef =
            DefinedMethod.EntryPoint (
                MemberVisibility.Public,
                MethodAttributes.None,
                MethodName.ofStr "Main",
                EntryPointKind.VoidNoArgs
            )

        let mainbody =
            { new DefinedMethodBody() with
                override _.WriteInstructions(wr, methods, _, types) =
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
