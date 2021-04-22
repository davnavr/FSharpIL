(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.ConstantsExample
#if !BENCHMARK
open Expecto

open System
open System.IO

open FSharpIL
#endif
#endif
(**
# Constants Example

The following example showcases the usage of pointer types.

The corresponding decompiled IL code is shown in C# instead of F#, as it is more accurate to the generated code.

## Example
*)
open FSharpIL.Metadata
open FSharpIL.PortableExecutable

let example() =
    let builder =
        { Name = Identifier.ofStr "PointersExample.dll"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    { Name = AssemblyName.ofStr "ConstantsExample"
      HashAlgId = ()
      Version = Version(1, 0, 0, 0)
      Flags = ()
      PublicKey = None
      Culture = NullCulture }
    |> Assembly.setRow builder
    |> ignore

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

    // public static class ConstantsExample
    let examples =
        { Access = TypeVisibility.Public
          Flags = ClassFlags(AutoLayout, AnsiClass) |> Flags.staticClass
          ClassName = Identifier.ofStr "ConstantsExample"
          TypeNamespace = String.Empty
          Extends = Extends.TypeRef object }
        |> StaticClass.addRow builder
    let examples' = StaticMemberOwner.StaticClass examples

    // public const double Pi = 3.14159265358979323846d;
    let pi =
        FloatConstant Math.PI
        |> builder.Blobs.Constant.GetOrAdd
        |> ConstantBlob.Float
    { Flags = LiteralFieldFlags Public
      FieldName = Identifier.ofStr "Pi"
      Signature = builder.Blobs.FieldSig.GetOrAdd(FieldSignature.create EncodedType.R8) }
    |> LiteralField.addRow builder examples' pi
    |> ignore

    let hello =
        StringConstant "Hello!"
        |> builder.Blobs.Constant.GetOrAdd
        |> ConstantBlob.String

    // public static void ShowMessage(string message)
    let showmsg =
        let body content =
            let wr = MethodBodyWriter content
            wr.Ret()
            MethodBody()
        let signature = StaticMethodSignature(ReturnType.itemVoid, ParamItem.create EncodedType.String)
        StaticMethod (
            MethodBody.create ValueNone body,
            Flags.staticMethod(StaticMethodFlags Public),
            Identifier.ofStr "ShowMessage",
            builder.Blobs.MethodDefSig.GetOrAdd signature
        )
        |> StaticMethod.addRow builder examples'

    // (string message = "Hello!")
    Parameter(ParamFlags(), "message", ValueSome hello) // TODO: When Constant table is sorted, move this below PI.
    |> Parameters.singleton builder (StaticMethod.methodIndex showmsg)
    |> ignore

    // public const string DefaultMessage = "Hello!";
    { Flags = LiteralFieldFlags Public
      FieldName = Identifier.ofStr "DefaultMessage"
      Signature = builder.Blobs.FieldSig.GetOrAdd(FieldSignature.create EncodedType.String) }
    |> LiteralField.addRow builder examples' hello
    |> ignore

    CliMetadata builder |> PEFile.ofMetadata ImageFileFlags.dll

(*** hide ***)
#if COMPILED && !BENCHMARK
[<Tests>]
let tests =
    let example' = lazy example()

    testList "constants example" [
        testCase "can save to disk" <| fun() ->
            let path = Path.Combine(__SOURCE_DIRECTORY__, "exout", "ConstantsExample.dll")
            WritePE.toPath path example'.Value
    ]
#endif
