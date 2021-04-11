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
        |> Assembly.setRow builder

    validated {
        let corver = Version(5, 0, 0, 0)
        let ptoken =
            PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
            |> builder.Blobs.MiscBytes.GetOrAdd
            |> PublicKeyOrToken

        let! mscorlib = AssemblyRef(corver, AssemblyName.ofStr "System.Runtime", ptoken) |> AssemblyRef.addRowChecked builder

        let mscorlib' = ResolutionScope.AssemblyRef mscorlib
        let! object = TypeRef.tryCreateReflectedRow builder mscorlib' typeof<Object>
        let! tfmattr = TypeRef.tryCreateReflectedRow builder mscorlib' typeof<System.Runtime.Versioning.TargetFrameworkAttribute>

        let! collections =
            AssemblyRef (
                corver,
                AssemblyName.ofStr "System.Collections",
                ptoken
            )
            |> AssemblyRef.addRowChecked builder
        let! dictionary =
            TypeRef.tryCreateReflectedRow
                builder
                (ResolutionScope.AssemblyRef collections)
                typedefof<System.Collections.Generic.Dictionary<_, _>>

        // System.Collections.Generic.Dictionary<uint32, uint32>
        let dictionary_u4_u4 = GenericInst(TypeDefOrRefOrSpecEncoded.TypeRef dictionary, false, EncodedType.U4, EncodedType.U4)
        let! dictionary_u4_u4_spec = TypeSpec.GenericInst dictionary_u4_u4 |> TypeSpec.tryCreateRow builder

        // member _.ContainsKey(_: 'TKey): bool
        let! containsKey =
            (* A default method reference is used here, since the generic parameters are declared in the dictionary type. *)
            { Class = MemberRefParent.TypeSpec dictionary_u4_u4_spec
              MemberName = Identifier.ofStr "ContainsKey"
              Signature =
                (* Note how a generic parameter is used here instead of using U4. *)
                let parameters = ImmutableArray.CreateRange [ ParamItem.var 0u; ]
                let signature = MethodRefDefaultSignature(true, false, ReturnType.itemBool, parameters)
                builder.Blobs.MethodRefSig.GetOrAdd signature }
            |> MethodRef.addRowDefaultChecked builder
        // member (*specialname*) _.get_Item(_: 'TKey): 'TValue
        let! get_Item =
            { Class = MemberRefParent.TypeSpec dictionary_u4_u4_spec
              MemberName = Identifier.ofStr "get_Item"
              Signature =
                let parameters = ImmutableArray.CreateRange [ ParamItem.var 0u ]
                let signature = MethodRefDefaultSignature(true, false, ReturnType.itemVar 1u, parameters)
                builder.Blobs.MethodRefSig.GetOrAdd signature }
            |> MethodRef.addRowDefaultChecked builder
        // new()
        let! dictionary_u4_u4_ctor =
            { Class = MemberRefParent.TypeSpec dictionary_u4_u4_spec
              MemberName = Identifier.ofStr ".ctor"
              Signature =
                let signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid, ImmutableArray.Empty)
                builder.Blobs.MethodRefSig.GetOrAdd signature }
            |> MethodRef.addRowDefaultChecked builder

        // [<AbstractClass; Sealed>] type CachedFactorial
        let! factorial =
            { ClassName = Identifier.ofStr "CachedFactorial"
              TypeNamespace = "Factorial"
              Access = TypeVisibility.Public
              Flags = Flags.staticClass(ClassFlags(AutoLayout, AnsiClass, beforeFieldInit = true))
              Extends = Extends.TypeRef object }
            |> StaticClass.tryAddRow builder
        let factorial' = StaticMemberOwner.StaticClass factorial

        // static member val private cache: System.Collections.Generic.Dictionary<uint32, uint32>
        let! cache =
            let row =
                { FieldName = Identifier.ofStr "cache"
                  Flags = Flags.staticField(FieldFlags Private)
                  Signature =
                    EncodedType.GenericInst dictionary_u4_u4
                    |> FieldSignature.create
                    |> builder.Blobs.FieldSig.GetOrAdd }
            StaticField.tryAddRow builder factorial' &row

        let calculateBody, setCalculateBody = MethodBody.mutableBody()

        // static member private CalculateHelper(num: uint32, accumulator: uint32): uint32
        let! helper =
            let parameters _ i =
                { Flags = ParamFlags()
                  ParamName =
                    match i with
                    | 0 -> "num"
                    | _ -> "accumulator" }
                |> Param
            let signature =
                StaticMethodSignature(
                    Default,
                    ReturnType.itemU4,
                    ImmutableArray.Create(ParamItem.create EncodedType.U4, ParamItem.create EncodedType.U4)
                )
            StaticMethod (
                calculateBody,
                Flags.staticMethod(StaticMethodFlags(Private, NoSpecialName, true)),
                Identifier.ofStr "CalculateHelper",
                builder.Blobs.MethodDefSig.GetOrAdd signature,
                parameters
            )
            |> StaticMethod.tryAddRow builder factorial'

        // static member Calculate(num: uint32): uint32
        let! _ =
            let body content =
                let writer = MethodBodyWriter content
                // CachedFactorial.cache.ContainsKey(num)
                writer.Ldsfld cache
                writer.Ldarg 0us
                writer.Callvirt containsKey // TODO: Figure out if callvirt is needed for ContainsKey and Add methods
                let target = writer.Brfalse_s()
                let pos = writer.ByteCount

                (* Get the existing value from the cache. *)
                // CachedFactorial.cache.[num]
                writer.Ldsfld cache
                writer.Ldarg 0us
                writer.Callvirt get_Item
                writer.Ret()

                target.SetTarget(int32 (writer.ByteCount - pos))

                // CachedFactorial.CacheHelper(num, num)
                writer.Ldarg 0us
                writer.Ldarg 0us
                writer.Call helper
                writer.Ret()
                MethodBody.Default
            let signature =
                StaticMethodSignature(
                    Default,
                    ReturnType.itemU4,
                    ParamItem.create EncodedType.U4 |> ImmutableArray.Create
                )
            StaticMethod (
                MethodBody.create ValueNone body,
                Flags.staticMethod(StaticMethodFlags(Public, NoSpecialName, true)),
                Identifier.ofStr "Calculate",
                builder.Blobs.MethodDefSig.GetOrAdd signature,
                fun _ _ -> Param { Flags = ParamFlags(); ParamName = "num" }
            )
            |> StaticMethod.tryAddRow builder factorial'

        (* Class constructor to initialize cache *)
        // static member (*specialname rtspecialname*) ``.cctor``(): System.Void
        let! _ =
            let body content =
                let writer = MethodBodyWriter content
                // CachedFactorial.cache <- new System.Collections.Generic.Dictionary<uint32, uint32>()
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
            |> ClassConstructor.tryAddRow builder factorial'

        let! tfm_ctor =
            { Class = MemberRefParent.TypeRef tfmattr
              MemberName = Identifier.ofStr ".ctor"
              Signature =
                let parameters = ImmutableArray.Create(ParamItem.create EncodedType.String)
                let signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid, parameters)
                builder.Blobs.MethodRefSig.GetOrAdd signature }
            |> MethodRef.addRowDefaultChecked builder

        Assembly.setTargetFramework builder assembly tfm_ctor ".NETCoreApp,Version=v5.0"

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
    |> ValidationResult.get // TODO: Returns warnings as well.
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
