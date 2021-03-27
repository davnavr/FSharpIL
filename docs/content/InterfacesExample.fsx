(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.InterfacesExample

open Expecto

open Swensen.Unquote

open System.IO

open Mono.Cecil

open FSharpIL
#endif
(**
# Interface Types Example

The following example showcases the generation of interface types.

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
        { Name = Identifier.ofStr "InterfacesExample.dll"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    { Name = AssemblyName.ofStr "InterfacesExample"
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

    // [<Interface>] type INumber
    let inumber =
        InterfaceDef (
            TypeVisibility.Public,
            Identifier.ofStr "INumber",
            "InterfacesExample"
        )
        |> Interface.addTypeDef builder

    // abstract Add: num: int32 -> INumber
    let inumber_add =
        let rtype =
            TypeDefOrRefOrSpecEncoded.InterfaceDef inumber
            |> EncodedType.Class
            |> ReturnType.encoded
        AbstractMethod (
            MethodBody.none,
            // TODO: Figure out how to stop users from saying isVirtual = false, note that Flags.abstractMethod ensures abstract methods are virtual anyway
            InstanceMethodFlags(Public, NoSpecialName, ReuseSlot, isVirtual = true) |> Flags.abstractMethod,
            Identifier.ofStr "Add",
            InstanceMethodSignature(rtype, ParamItem.create EncodedType.I4),
            Param { Flags = ParamFlags(); ParamName = "num" } |> ParamList.singleton
        )
        |> Interface.addAbstractMethod builder inumber

    CliMetadata builder |> PEFile.ofMetadata ImageFileFlags.dll

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    let example' = lazy example()
    let metadata = lazy PEFile.toCecilModule example'.Value

    afterRunTests <| fun() -> if metadata.IsValueCreated then metadata.Value.Dispose()

    testList "interfaces example" [
        testCase "can save to disk" <| fun() ->
            let path = Path.Combine(__SOURCE_DIRECTORY__, "exout", "InterfacesExample.dll")
            WritePE.toPath path example'.Value
    ]
#endif
