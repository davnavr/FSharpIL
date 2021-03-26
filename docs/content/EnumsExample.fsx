(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.EnumsExample

open Expecto
open Swensen.Unquote

open System.IO

open Mono.Cecil

open FSharpIL
#endif
(**
# Enumeration types Example

The following example showcases the generation and usage of enumeration types.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Unchecked
open FSharpIL.Metadata.UncheckedExn
open FSharpIL.Metadata.CliMetadata
open FSharpIL.PortableExecutable

let example() =
    let builder =
        { Name = Identifier.ofStr "EnumsExample.dll"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    { Name = AssemblyName.ofStr "EnumsExample"
      HashAlgId = ()
      Version = Version(69, 110, 117, 109)
      Flags = ()
      PublicKey = None
      Culture = NullCulture }
    |> setAssembly builder
    |> ignore

    let struct (mscorlib, _) =
        { Version = Version(5, 0, 0, 0)
          PublicKeyOrToken = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
          Name = AssemblyName.ofStr "System.Runtime"
          Culture = NullCulture
          HashValue = None }
        |> referenceAssembly builder
    let object =
        { TypeName = Identifier.ofStr "Object"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder
    let enum =
        { TypeName = Identifier.ofStr "Enum"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder

    (* Generating Enumeration Types *)
    let myenum_values = EnumValueListBuilder(IntegerType.I4, 4)

    // | A = 0
    myenum_values.TryAdd(Identifier.ofStr "A", IntegerConstant 0) |> ignore

    // | B = 1
    myenum_values.TryAdd(Identifier.ofStr "B", IntegerConstant 1) |> ignore

    // | C = 256
    myenum_values.TryAdd(Identifier.ofStr "C", IntegerConstant 256) |> ignore

    // | D = -3
    myenum_values.TryAdd(Identifier.ofStr "D", IntegerConstant -3) |> ignore

    // | E = -512
    myenum_values.TryAdd(Identifier.ofStr "E", IntegerConstant -512) |> ignore

    // | F = 12345678
    myenum_values.TryAdd(Identifier.ofStr "F", IntegerConstant 12345678) |> ignore

    // type MyEnum
    let myenum =
        Unsafe.AddEnum (
            builder,
            enum,
            EnumDef (
                TypeVisibility.Public,
                myenum_values.ToImmutable(),
                Identifier.ofStr "MyEnum",
                ns = "EnumsExample",
                serializable = true
            )
        )

    (* Using Enumeration Types *)

    CliMetadata builder |> PEFile.ofMetadata ImageFileFlags.dll

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    let example' = lazy example()
    let metadata = lazy PEFile.toCecilModule example'.Value

    afterRunTests <| fun() -> if metadata.IsValueCreated then metadata.Value.Dispose()

    testList "enums example" [
        testCase "can save to disk" <| fun() ->
            let path = Path.Combine(__SOURCE_DIRECTORY__, "exout", "EnumsExample.dll")
            WritePE.toPath path example'.Value

        testCase "enum has correct field values" <| fun() ->
            let expected = List.map Some [ 0; 1; 256; -3; -512; 12345678 ]
            let actual =
                metadata.Value.GetType("EnumsExample", "MyEnum").Fields
                |> Seq.where (fun field -> field.Attributes.HasFlag FieldAttributes.Static)
                |> Seq.map (fun field -> Option.ofObj field.Constant |> Option.map unbox<int32>)
                |> List.ofSeq
            expected =! actual
    ]
#endif
