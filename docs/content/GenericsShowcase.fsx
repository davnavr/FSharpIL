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
open FSharpIL.Metadata.Cil
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
        // type public [<Sealed>] ArrayList<'T> = inherit System.Object
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

        let t = CliType.TypeVar arrlist.Parameters.[0]

        let! members = builder.DefineGenericType(arrlist, attributes = ValueNone)

        // val private items: 'T[]
        let! items =
            let definition =
                DefinedField.Instance (
                    MemberVisibility.Private,
                    flags = FieldAttributes.InitOnly,
                    name = Identifier.ofStr "items",
                    signature = CliType.SZArray(CliType.TypeVar arrlist.Parameters.[0])
                )

            members.Members.DefineField(definition, attributes = ValueNone)

        let items' = builder.GenericInstantiation(false, items.Token, fun _ _ -> t)

        // val mutable index: int32
        let! index =
            let definition =
                DefinedField.Instance (
                    MemberVisibility.Private,
                    flags = FieldAttributes.None,
                    name = Identifier.ofStr "index",
                    signature = PrimitiveType.I4
                )

            members.Members.DefineField(definition, attributes = ValueNone)

        let index' = builder.GenericInstantiation(false, index.Token, fun _ _ -> t)

        // public new: capacity: int32 -> GenericsShowcase.ArrayList<'T>
        let! ctor =
            let definition =
                DefinedMethod.Constructor (
                    MemberVisibility.Public,
                    flags = MethodAttributes.None,
                    parameterTypes = ImmutableArray.Create(ParameterType.T PrimitiveType.I4),
                    parameterList = fun _ _ -> Identifier.ofStr "capacity" |> Parameter.named
                )

            let body = MethodBody.ofSeq [
                // inherit System.Object()
                ldarg_0
                callvirt mscorlib'.ObjectConstructor.Token

                // inner = Microsoft.FSharp.Primitives.Basics.Array.zeroCreateUnchecked capacity
                ldarg_0
                ldarg_1
                newarr (TypeTok.Specified t)
                stfld items'

                // index = 0
                ldarg_0
                ldc_i4_0
                stfld index'

                ret
            ]

            members.DefineMethod(definition, body, attributes = ValueNone)

        // member public Add: item: 'T -> unit
        let! add =
            let definition =
                DefinedMethod.Instance (
                    MemberVisibility.Public,
                    flags = MethodAttributes.None,
                    returnType = ReturnType.Void',
                    name = MethodName.ofStr "Add",
                    parameterTypes = ImmutableArray.Create(ParameterType.T t),
                    parameterList = fun _ _ -> Identifier.ofStr "item" |> Parameter.named
                )

            let locals =
                ImmutableArray.CreateRange [
                    CliType.toLocalType PrimitiveType.I4 // let length: int32
                ]

            let body = MethodBody.create InitLocals ValueNone (LocalVariables.Locals locals) [
                let struct(addItemLabel, addItem) =
                    InstructionBlock.ofList [
                        // this.items.[this.index] <- item
                        ldarg_0
                        ldfld items'
                        ldarg_0
                        ldfld index'
                        ldarg_1
                        stelem (TypeTok.Specified t)

                        // this.index <- this.index + 1
                        ldarg_0
                        dup
                        ldfld index'
                        ldc_i4_1
                        add
                        stfld index'

                        ret
                    ]
                    |> InstructionBlock.label

                InstructionBlock.ofList [
                    // let mutable length = this.items.Length
                    ldarg_0
                    ldfld items'
                    ldlen
                    conv_i4
                    stloc_0

                    // this.index > length
                    ldarg_0
                    ldfld index'
                    ldloc_0
                    bgt_s addItemLabel

                    // TODO: Do some resize stuff here.
                    ldarg_0 // TEMPORARY
                    pop // TEMPORARY
                ]

                addItem
            ]

            members.DefineMethod(definition, body, attributes = ValueNone)

        // static member public Main: unit -> unit
        let! _ =
            let main =
                DefinedMethod.EntryPoint (
                    MemberVisibility.Public,
                    MethodAttributes.None,
                    MethodName.ofStr "Main",
                    EntryPointKind.VoidNoArgs
                )

            let istring _ _ = PrimitiveType.String

            let locals =
                ImmutableArray.CreateRange [
                    // let strings: GenericsShowcase.ArrayList<string>
                    GenericType.instantiate (GenericType.Defined arrlist.Definition) istring
                    |> CliType.GenericClass
                    |> CliType.toLocalType
                ]

            let body = MethodBody.create InitLocals ValueNone (LocalVariables.Locals locals) [
                InstructionBlock.ofList [
                    // let strings = new GenericsShowcase.ArrayList<string>(4)
                    ldc_i4_4
                    Newobj.ofMethod (builder.GenericInstantiation(false, ctor.Token, istring))
                    stloc_0

                    // strings.Add("Hello!")
                    let add' = builder.GenericInstantiation(false, add.Token, istring)
                    ldloc_0
                    ldstr "Hello!"
                    call add'

                    // strings.Add("How are you?")
                    ldloc_0
                    ldstr "How are you?"
                    call add'

                    ret
                ]
            ]

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
