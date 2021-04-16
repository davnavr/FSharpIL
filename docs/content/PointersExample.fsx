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
        StaticMethodSignature ReturnType.itemVoid
    StaticMethod (
        managed1_body,
        StaticMethodFlags(Public, NoSpecialName) |> Flags.staticMethod,
        name = Identifier.ofStr "ManagedPointersExample",
        signature = builder.Blobs.MethodDefSig.GetOrAdd managed1_sig // TODO: Create byref parameters................................................................
    )
    |> StaticMethod.addRow builder examples'
    |> ignore<RawIndex<_>>

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
