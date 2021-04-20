(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.PointersExample
#if !BENCHMARK
open Expecto

open System
open System.IO

open FSharpIL
#endif
#endif
(**
# Points Example

The following example showcases the usage of pointer types.

Because the version of F# used does not currently have syntac for function pointers, the corresponding decompiled IL code is shown
in C# instead.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

let example() =
    let builder =
        { Name = Identifier.ofStr "PointersExample.dll"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    let assembly =
        { Name = AssemblyName.ofStr "PointersExample"
          HashAlgId = ()
          Version = Version(1, 0, 0, 0)
          Flags = ()
          PublicKey = None
          Culture = NullCulture }
        |> Assembly.setRow builder

    let struct (mscorlib, _) =
        let token =
            PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
            |> builder.Blobs.MiscBytes.GetOrAdd
            |> PublicKeyOrToken
        AssemblyRef (
            Version(5, 0, 0, 0),
            AssemblyName.ofStr "System.Runtime",
            token
        )
        |> AssemblyRef.addRow builder
    let mscorlib' = ResolutionScope.AssemblyRef mscorlib

    let object = TypeRef.createReflectedRow builder mscorlib' typeof<Object>

    // public class PointerExamples
    let examples =
        { Access = TypeVisibility.Public
          Flags = ClassFlags(AutoLayout, AnsiClass) |> Flags.staticClass
          ClassName = Identifier.ofStr "PointerExamples"
          TypeNamespace = String.Empty
          Extends = Extends.TypeRef object }
        |> StaticClass.addRow builder
    let examples' = StaticMemberOwner.StaticClass examples

    (* Native Pointers *)
    let native1_body =
        fun content ->
            let wr = MethodBodyWriter content
            wr.Ret()
            MethodBody.Default
        |> MethodBody.create ValueNone
    let native1_sig =
        let parameters =
            [|
                EncodedType.Ptr(Pointer.toType EncodedType.I4)
                EncodedType.Ptr(Pointer.Void ImmutableArray.Empty)
            |]
            |> Array.map ParamItem.create
        StaticMethodSignature(ReturnType.itemVoid, parameters)
    // public unsafe static void NativePointersExample(int* P_0, void* P_1)
    StaticMethod (
        native1_body,
        StaticMethodFlags(Public, NoSpecialName) |> Flags.staticMethod,
        name = Identifier.ofStr "NativePointersExample",
        signature = builder.Blobs.MethodDefSig.GetOrAdd native1_sig
    )
    |> StaticMethod.addRow builder examples'
    |> ignore<RawIndex<_>>

    (* Managed Pointers *)
    let managed1_body =
        //let locals =
        //    EncodedType.String
        //    |> LocalVariable.ByRef ImmutableArray.Empty ImmutableArray.Empty
        //    |> ImmutableArray.Create
        //    |> builder.Blobs.LocalVarSig.GetOrAdd
        //    |> builder.StandAloneSig.AddLocals
        //    |> ValueSome
        fun content ->
            let wr = MethodBodyWriter content
            wr.Ret()
            MethodBody.Default
        |> MethodBody.create ValueNone
    let managed1_sig =
        StaticMethodSignature(ReturnType.itemVoid, ParamItem.byRef EncodedType.String)
    // public static void ManagedPointersExample(ref string P_0)
    StaticMethod (
        managed1_body,
        StaticMethodFlags(Public, NoSpecialName) |> Flags.staticMethod,
        name = Identifier.ofStr "ManagedPointersExample",
        signature = builder.Blobs.MethodDefSig.GetOrAdd managed1_sig
    )
    |> StaticMethod.addRow builder examples'
    |> ignore<RawIndex<_>>

    (* Function Pointers *)
    let mapper =
        // TODO: Should default or generic calling convention be used?
        MethodRefGenericSignature (
            false,
            false,
            2u,
            ReturnType.encoded(EncodedType.MVar 1u),
            ImmutableArray.Create(ParamItem.mvar 0u)
        )
        |> builder.Blobs.MethodRefSig.GetOrAdd
        |> MethodRefSignature
        |> FunctionPointer.Ref
    let mapper' = builder.StandAloneSig.AddSignature mapper

    let tto = TypeSpec.MVar 1u |> TypeSpec.createRow builder

    // public static TTo[] MapArray<TFrom, TTo>(delegate*<TFrom, TTo> mapper, TFrom[] array)
    let maparray =
        let garray num = EncodedType.SZArray(ImmutableArray.Empty, EncodedType.MVar num)
        let body =
            let locals =
                ImmutableArray.Create (
                    LocalVariable.encoded(garray 1u), // TTo[] result;
                    LocalVariable.encoded EncodedType.I4 // int i;
                )
                |> builder.Blobs.LocalVarSig.GetOrAdd
                |> builder.StandAloneSig.AddLocals
                |> ValueSome
            fun content ->
                let wr = MethodBodyWriter content
                // result = new TTo[array.Length];
                wr.Ldarg 1us
                wr.Ldlen()
                wr.Conv_i4()
                wr.Newarr tto
                wr.Stloc 0us

                // i = 0;
                wr.Ldc_i4 0
                wr.Stloc 1us

                (* go to condition of loop *)
                let start = wr.Br_s()
                let start_offset = wr.ByteCount

                (* body of loop *)
                let lbody = Label wr
                // result[i] = mapper(array[i]);
                wr.Ldloc 0us
                wr.Ldloc 1us
                wr.Ldarg 1us
                wr.Ldloc 0us
                wr.Calli mapper'
                wr.Stelem tto

                // i += 1;
                wr.Ldloc 1us
                wr.Ldc_i4 1
                wr.Add()
                wr.Stloc 1us

                start.SetTarget(int32 (wr.ByteCount - start_offset))
                // i < array.Length
                wr.Ldloc 1us
                wr.Ldarg 1us
                wr.Ldlen()
                wr.Conv_i4()

                (*go to start if true*)
                let go = wr.Blt_s()
                lbody.SetTarget go

                // return result;
                wr.Ldloc 0us
                wr.Ret()
                MethodBody(4us, true)
            |> MethodBody.create locals
        let parameters =
            ImmutableArray.Create (
                ParamItem.create(EncodedType.FnPtr mapper), // delegate*<TFrom, TTo> mapper
                ParamItem.create(garray 0u) // TFrom[] array
            )
        let signature =
            StaticMethodSignature(MethodCallingConventions.Generic 2u, ReturnType.encoded(garray 1u), parameters)
        StaticMethod (
            body,
            StaticMethodFlags(Public, NoSpecialName) |> Flags.staticMethod,
            name = Identifier.ofStr "MapArray",
            signature = builder.Blobs.MethodDefSig.GetOrAdd signature
        )
        |> StaticMethod.addRow builder examples'

    Parameters.named builder (StaticMethod.methodIndex maparray) [| "mapper"; "array" |] |> ignore

    // TFrom
    GenericParam.createRow
        builder
        (StaticMethod.methodIndex maparray |> GenericParamOwner.MethodDef)
        GenericParamFlags.None
        (Identifier.ofStr "TFrom")
        Invariant
        ConstraintSet.empty
    |> ignore

    // TTo
    GenericParam.createRow
        builder
        (StaticMethod.methodIndex maparray |> GenericParamOwner.MethodDef)
        GenericParamFlags.None
        (Identifier.ofStr "TTo")
        Invariant
        ConstraintSet.empty
    |> ignore

    CliMetadata builder |> PEFile.ofMetadata ImageFileFlags.dll

(*** hide ***)
#if COMPILED && !BENCHMARK
[<Tests>]
let tests =
    let example' = lazy example()

    testList "pointers example" [
        testCase "can save to disk" <| fun() ->
            let path = Path.Combine(__SOURCE_DIRECTORY__, "exout", "PointersExample.dll")
            WritePE.toPath path example'.Value
    ]
#endif
