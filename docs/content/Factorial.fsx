(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.Factorial

open Expecto

open Swensen.Unquote

open System.IO

open FSharpIL
#endif
(**
# Factorial

The following example showcases the use of tail calls and the usage of methods on a generic class.

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
        { Name = Identifier.ofStr "Factorial.dll"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    let assembly =
        { Name = AssemblyName.ofStr "Factorial"
          HashAlgId = ()
          Version = Version(1, 2, 6, 24)
          Flags = ()
          PublicKey = None
          Culture = NullCulture }
        |> setAssembly builder

    // TODO: Add target framework attribute to Factorial example.
    validated {
        let! mscorlib =
            { Version = Version(5, 0, 0, 0)
              PublicKeyOrToken = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
              Name = AssemblyName.ofStr "System.Runtime"
              Culture = NullCulture
              HashValue = None }
            |> referenceAssembly builder
        let! object = SystemType.object builder mscorlib
        let! tfmAttr =
            { TypeName = Identifier.ofStr "TargetFrameworkAttribute"
              TypeNamespace = "System.Runtime.Versioning"
              ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
            |> referenceType builder

        let! collections =
            { Version = Version(5, 0, 0, 0)
              PublicKeyOrToken = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
              Name = AssemblyName.ofStr "System.Collections"
              Culture = NullCulture
              HashValue = None }
            |> referenceAssembly builder
        let! dictionary =
            { ResolutionScope = ResolutionScope.AssemblyRef collections
              TypeName = Identifier.ofStr "Dictionary`2"
              TypeNamespace = "System.Collections.Generic" }
            |> referenceType builder

        let dictionary_u4_u4 =
            GenericInst(TypeDefOrRefOrSpecEncoded.TypeRef dictionary, false, EncodedType.U4, EncodedType.U4)
        let! dictionary_u4_u4_spec = TypeSpec.genericInst dictionary_u4_u4 |> addTypeSpec builder

        let! containsKey =
            // A default method reference is used here, since the generic parameters are declared in the dictionary type.
            { Class = MemberRefParent.TypeSpec dictionary_u4_u4_spec
              MemberName = Identifier.ofStr "ContainsKey"
              Signature =
                let parameters =
                    // Note how a generic parameter is used here instead of using U4.
                    ImmutableArray.CreateRange [ ParamItem.var 0u; ]
                MethodRefDefaultSignature(true, false, ReturnType.itemBool, parameters) }
            |> referenceDefaultMethod builder
        let! get_Item =
            { Class = MemberRefParent.TypeSpec dictionary_u4_u4_spec
              MemberName = Identifier.ofStr "get_Item"
              Signature =
                let parameters = ImmutableArray.CreateRange [ ParamItem.var 0u ]
                MethodRefDefaultSignature(true, false, ReturnType.itemVar 1u, parameters) }
            |> referenceDefaultMethod builder
        let! dictionary_u4_u4_ctor =
            { Class = MemberRefParent.TypeSpec dictionary_u4_u4_spec
              MemberName = Identifier.ofStr ".ctor"
              Signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid, ImmutableArray.Empty) }
            |> referenceDefaultMethod builder

        // [<AbstractClass; Sealed>] type CachedFactorial
        let! factorial =
            { ClassName = Identifier.ofStr "CachedFactorial"
              TypeNamespace = "Factorial"
              Access = TypeVisibility.Public
              Flags = Flags.staticClass(ClassFlags(AutoLayout, AnsiClass, beforeFieldInit = true))
              Extends = Extends.TypeRef object }
            |> StaticClass.addTypeDef builder

        let! cache =
            { FieldName = Identifier.ofStr "cache"
              Flags = Flags.staticField(FieldFlags Private)
              Signature = EncodedType.GenericInst dictionary_u4_u4 |> FieldSignature.create }
            |> StaticClass.addStaticField builder factorial

        let calculateBody, setCalculateBody = MethodBody.mutableBody()

        let! helper =
            let parameters _ i =
                { Flags = ParamFlags()
                  ParamName =
                    match i with
                    | 0 -> "num"
                    | _ -> "accumulator" }
                |> Param
            StaticMethod (
                calculateBody,
                Flags.staticMethod(StaticMethodFlags(Public, NoSpecialName, true)),
                Identifier.ofStr "CalculateHelper",
                StaticMethodSignature(
                    Default,
                    ReturnType.itemU4,
                    ImmutableArray.Create(ParamItem.create EncodedType.U4, ParamItem.create EncodedType.U4)
                ),
                parameters
            )
            |> StaticClass.addStaticMethod builder factorial

        let! _ =
            let body content =
                let writer = MethodBodyWriter content
                writer.Ldsfld cache
                writer.Ldarg 0us
                writer.Callvirt containsKey // TODO: Figure out if callvirt is needed for ContainsKey and Add methods
                let target = writer.Brfalse_s()
                let pos = writer.ByteCount

                // Get the existing value from the cache.
                writer.Ldsfld cache
                writer.Ldarg 0us
                writer.Callvirt get_Item
                writer.Ret()

                target.SetTarget(int32 (writer.ByteCount - pos))

                writer.Ldarg 0us
                writer.Ldarg 0us
                writer.Call helper
                writer.Ret()
                MethodBody.Default
            StaticMethod (
                MethodBody.create ValueNone body,
                Flags.staticMethod(StaticMethodFlags(Public, NoSpecialName, true)),
                Identifier.ofStr "Calculate",
                StaticMethodSignature(
                    Default,
                    ReturnType.itemU4,
                    ParamItem.create EncodedType.U4 |> ImmutableArray.Create
                ),
                fun _ _ -> Param { Flags = ParamFlags(); ParamName = "num" }
            )
            |> StaticClass.addStaticMethod builder factorial

        // Class constructor to initialize cache
        let! _ =
            let body content =
                let writer = MethodBodyWriter content
                writer.Newobj dictionary_u4_u4_ctor
                writer.Stsfld cache
                writer.Ret()
                MethodBody.Default
            Constructor(
                MethodBody.create ValueNone body,
                MethodImplFlags(),
                ConstructorFlags(Public, true) |> Flags.classConstructor,
                (),
                ParamList.empty
            )
            |> StaticClass.addClassConstructor builder factorial

        let! tfm_ctor =
            { Class = MemberRefParent.TypeRef tfmAttr
              MemberName = Identifier.ofStr ".ctor"
              Signature =
                let parameters = ImmutableArray.Create(ParamItem.create EncodedType.String)
                MethodRefDefaultSignature(true, false, ReturnType.itemVoid, parameters) }
            |> referenceDefaultMethod builder

        setTargetFramework builder assembly tfm_ctor ".NETCoreApp,Version=v5.0"

        setCalculateBody <| fun content ->
            let writer = MethodBodyWriter content
            writer.Ldarg 0us
            writer.Ldc_i4 1
            let target = writer.Bgt_un_s() // If "num" <= 1, return the accumulator value.
            let pos = writer.ByteCount

            writer.Ldarg 1us
            writer.Ret()

            target.SetTarget(int32 (writer.ByteCount - pos))

            writer.Ldarg 0us
            writer.Ldc_i4 1
            writer.Conv_u4()
            writer.Sub()

            // TODO: Maybe store this in a local variable instead of duplicating it.
            writer.Ldarg 0us
            writer.Ldc_i4 1
            writer.Conv_u4()
            writer.Sub()

            writer.Ldarg 1us
            writer.Mul()
            writer.Tail_call helper
            writer.Ret()
            MethodBody.Default

        return CliMetadata builder
    }
    |> ValidationResult.get
    |> PEFile.ofMetadata ImageFileFlags.dll

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    let example' = lazy example()
    let metadata = lazy PEFile.toCecilModule example'.Value

    afterRunTests <| fun() -> metadata.Value.Dispose()

    // TODO: Shuffle tests?
    testList "factorial" [
        testCase "has correct field names" <| fun() ->
            let expected = [ "cache" ]
            let types = metadata.Value.Types
            let actual =
                types
                |> Seq.collect (fun tdef -> tdef.Fields)
                |> Seq.map (fun field -> field.Name)
                |> List.ofSeq
            expected =! actual

        testCase "can save to disk" <| fun() ->
            let path = Path.Combine(__SOURCE_DIRECTORY__, "exout", "Factorial.dll")
            WritePE.toPath path example'.Value
    ]
#endif
