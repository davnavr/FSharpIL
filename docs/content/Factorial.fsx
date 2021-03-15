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
        let! mscorlib = SystemAssembly.Net5_0.private_corelib builder
        let! object = SystemType.object builder mscorlib
        let! tfmAttr =
            { TypeName = Identifier.ofStr "TargetFrameworkAttribute"
              TypeNamespace = "System.Runtime.Versioning"
              ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
            |> referenceType builder

        let! dictionary =
            { ResolutionScope = ResolutionScope.AssemblyRef mscorlib
              TypeName = Identifier.ofStr "Dictionary`2"
              TypeNamespace = "System.Collections.Generic" }
            |> referenceType builder

        let dictionary_u4_u4 = GenericInst.typeRef false dictionary [ EncodedType.U4; EncodedType.U4 ]
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
            |> StaticField
            :> IField<StaticClassDef>
            |> addField builder factorial

        let calculateBody, setCalculateBody = MethodBody.mutableBody()

        let! helper =
            { MethodName = Identifier.ofStr "CalculateHelper"
              ImplFlags = MethodImplFlags()
              Flags = Flags.staticMethod(StaticMethodFlags(Public, NoSpecialName, true))
              ParamList =
                fun _ i ->
                    { Flags = ParamFlags()
                      ParamName =
                        match i with
                        | 0 -> "num"
                        | _ -> "accumulator" }
                    |> Param
              Signature =
                StaticMethodSignature(
                    Default,
                    ReturnType.itemU4,
                    ImmutableArray.Create(ParamItem.create EncodedType.U4, ParamItem.create EncodedType.U4)
                )
              Body = calculateBody }
            |> StaticClass.addStaticMethod builder factorial

        let! _ =
            { MethodName = Identifier.ofStr "Calculate"
              ImplFlags = MethodImplFlags()
              Flags = Flags.staticMethod(StaticMethodFlags(Public, NoSpecialName, true))
              // TODO: Figure out why the parameter name is not correct in the decompiler
              ParamList = fun _ _ -> Param { Flags = ParamFlags(); ParamName = "num" }
              Signature =
                StaticMethodSignature(
                    Default,
                    ReturnType.itemU4,
                    ParamItem.create EncodedType.U4 |> ImmutableArray.Create
                )
              Body =
                fun content ->
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
                    { MaxStack = 8us; InitLocals = false }
                |> MethodBody.create }
            |> StaticClass.addStaticMethod builder factorial

        // Class constructor to initialize cache
        let! _ =
            let body content =
                let writer = MethodBodyWriter content
                writer.Newobj dictionary_u4_u4_ctor
                writer.Stsfld cache
                writer.Ret()
                { MaxStack = 8us; InitLocals = false }
            Constructor(
                MethodBody.create body,
                MethodImplFlags(),
                ConstructorFlags(Public, true) |> Flags.classConstructor,
                (),
                fun _ _ -> failwith "class constructor has no arguments"
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
            writer.conv_u4()
            writer.Sub()

            // TODO: Maybe store this in a local variable instead of duplicating it.
            writer.Ldarg 0us
            writer.Ldc_i4 1
            writer.conv_u4()
            writer.Sub()

            writer.Ldarg 1us
            writer.Mul()
            writer.Tail_call helper
            writer.Ret()
            { MaxStack = 8us; InitLocals = false }

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
    let context =
        lazy
            let (ctx, assembly) = PEFile.toLoadContext "factorial" example'.Value
            let calculate = assembly.GetType("Factorial.CachedFactorial").GetMethod("Calculate")
            {| Context = ctx; Assembly = assembly; Calculate = calculate |}

    afterRunTests <| fun() ->
        context.Value.Context.Unload()
        metadata.Value.Dispose()

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

        // TODO: Use a property test can be used to test factorial calculation.
        testCase "method can be called" <| fun () ->
            let expected = 6u
            let actual = context.Value.Calculate.Invoke(null, [| 3u |]) |> unbox
            expected =! actual
    ]
#endif
