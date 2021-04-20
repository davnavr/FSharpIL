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
open System
open System.Collections.Immutable

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

    // public static class Math
    let math =
        { Access = TypeVisibility.Public
          Flags = ClassFlags(AutoLayout, AnsiClass) |> Flags.staticClass
          ClassName = Identifier.ofStr "Math"
          TypeNamespace = "CustomNumbers"
          Extends = Extends.TypeRef object }
        |> StaticClass.addRow builder
    let math' = StaticMemberOwner.StaticClass math

    // 3.14159265358979323846d
    let pi =
        let value =
            FloatConstant Math.PI
            |> builder.Blobs.Constant.GetOrAdd
            |> ConstantBlob.Float
        { Flags = LiteralFieldFlags Public
          FieldName = Identifier.ofStr "Pi"
          Signature = builder.Blobs.FieldSig.GetOrAdd(FieldSignature.create EncodedType.R8) }
        |> LiteralField.addRow builder math' value

    // TODO: Add example that uses default parameter value.

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
