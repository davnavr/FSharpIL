(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.CustomCollections

open Expecto

open Swensen.Unquote

open System.IO

open FSharpIL
#endif
(**
# Custom Collections

The following example showcases the generation of generic parameters.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.Metadata.Unchecked
open FSharpIL.Metadata.UncheckedExn
open FSharpIL.PortableExecutable

let example() =
    let builder =
        { Name = Identifier.ofStr "CustomCollections.dll"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    let assembly =
        { Name = AssemblyName.ofStr "CustomCollections"
          HashAlgId = ()
          Version = Version(1, 23, 456, 7890)
          Flags = ()
          PublicKey = None
          Culture = NullCulture }
        |> setAssembly builder

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

    let myCollection =
        { Access = TypeVisibility.Public
          Flags = ClassFlags(AutoLayout, AnsiClass, beforeFieldInit = true) |> Flags.concreteClass
          // C#, VB, and F# compilers append the number of generic parameters to the class name
          ClassName = Identifier.ofStr "MyCollection`1"
          TypeNamespace = "Example"
          Extends = Extends.TypeRef object }
        |> ConcreteClass.addTypeDef builder

    let struct(g_t, _) =
        let owner = myCollection.AsTypeIndex() |> GenericParamOwner.TypeDef
        GenericParam.addNonvariant builder GenericParamFlags.None owner (Identifier.ofStr "T") ConstraintSet.empty

    let tfm =
        { TypeName = Identifier.ofStr "TargetFrameworkAttribute"
          TypeNamespace = "System.Runtime.Versioning"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder

    let struct(tfm_ctor, _) =
        { Class = MemberRefParent.TypeRef tfm
          MemberName = Identifier.ofStr ".ctor"
          Signature =
            let parameters = ImmutableArray.Create(ParamItem.create EncodedType.String)
            MethodRefDefaultSignature(true, false, ReturnType.itemVoid, parameters) }
        |> referenceDefaultMethod builder

    setTargetFramework builder assembly tfm_ctor ".NETCoreApp,Version=v5.0"

    CliMetadata builder |> PEFile.ofMetadata ImageFileFlags.dll

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    let example' = lazy example()
    let metadata = lazy PEFile.toCecilModule example'.Value

    afterRunTests <| fun() -> if metadata.IsValueCreated then metadata.Value.Dispose()

    testList "custom collection types" [
        testCase "can save to disk" <| fun() ->
            let path = Path.Combine(__SOURCE_DIRECTORY__, "exout", "CustomCollections.dll")
            WritePE.toPath path example'.Value
    ]
#endif
