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
    let struct (object_ctor, _) =
        { Class = MemberRefParent.TypeRef object
          MemberName = Identifier.ofStr ".ctor"
          Signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid) }
        |> referenceDefaultMethod builder

    // type MyCollection
    let myCollection =
        { Access = TypeVisibility.Public
          Flags = ClassFlags(AutoLayout, AnsiClass, beforeFieldInit = true) |> Flags.concreteClass
          // C#, VB, and F# compilers append the number of generic parameters to the class name
          ClassName = Identifier.ofStr "MyCollection`1"
          TypeNamespace = "Example"
          Extends = Extends.TypeRef object }
        |> ConcreteClass.addTypeDef builder

    // 'T
    GenericParam.addNonvariant
        builder
        GenericParamFlags.None
        (myCollection.AsTypeIndex() |> GenericParamOwner.TypeDef)
        (Identifier.ofStr "T")
        ConstraintSet.empty
    |> ignore

    let tparam = TypeSpec.Var 0u |> TypeSpec.create |> addTypeSpec builder
    let tencoded = EncodedType.Var 0u

    // val private (*initonly*) items: 'T[]
    let items =
        { Flags = FieldFlags(Private, initOnly = true) |> Flags.instanceField
          FieldName = Identifier.ofStr "items"
          Signature = EncodedType.SZArray(ImmutableArray.Empty, tencoded) |> FieldSignature.create }
        |> ConcreteClass.addInstanceField builder myCollection
    // val mutable private index: int32
    let index =
        { Flags = Flags.instanceField(FieldFlags Private)
          FieldName = Identifier.ofStr "index"
          Signature = FieldSignature.create EncodedType.I4 }
        |> ConcreteClass.addInstanceField builder myCollection

    // TODO: Need to make typespec for MyCollection<!T>
    let myCollectionSpec =
        GenericInst.typeDef1 false (myCollection.AsTypeIndex()) tencoded
        |> TypeSpec.genericInst
        |> addTypeSpec builder

    let ctor_p_params =
        [|
            EncodedType.SZArray(ImmutableArray.Empty, tencoded) |> ParamItem.create
            ParamItem.create EncodedType.I4
        |]

    // private new (items: 'T[], index: int32)
    let ctor_p =
        let body content =
            let wr = MethodBodyWriter content
            wr.Ldarg 0us
            wr.Call object_ctor
            // { items = items;
            wr.Ldarg 0us
            wr.Ldarg 1us
            wr.Stfld items // TODO: Use field ref
            // index = 0 }
            wr.Ldarg 0us
            wr.Ldarg 2us
            wr.Stfld index
            wr.Ret()
            { MaxStack = 2us; InitLocals = false }
        Constructor (
            body = MethodBody.create body,
            implFlags = MethodImplFlags(),
            flags = (ConstructorFlags(Private, true) |> Flags.constructor),
            signature = ObjectConstructorSignature(parameters = ctor_p_params),
            paramList = fun _ i -> Param { Flags = ParamFlags(); ParamName = if i = 0 then "items" else "index" }
        )
        |> ConcreteClass.addConstructor builder myCollection

    let struct(ctor_p', _) =
        { MemberRef.MemberName = Identifier.ofStr ".ctor"
          Class = MemberRefParent.TypeSpec myCollectionSpec
          Signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid, ctor_p_params) }
        |> builder.MemberRef.Add

    // new (capacity: int32)
    let ctor =
        let body content =
            let wr = MethodBodyWriter content
            // = MyCollection(Array.zeroCreate<'T> capacity, 0)
            wr.Ldarg 0us
            wr.Ldarg 1us
            wr.Newarr tparam
            wr.Ldc_i4 0y
            wr.Call ctor_p'
            wr.Ret()
            { MaxStack = 3us; InitLocals = false }
        Constructor (
            body = MethodBody.create body,
            implFlags = MethodImplFlags(),
            flags = (ConstructorFlags(Public, true) |> Flags.constructor),
            signature = ObjectConstructorSignature(ParamItem.create EncodedType.I4),
            paramList = fun _ _ -> Param { Flags = ParamFlags(); ParamName = "capacity" }
        )
        |> ConcreteClass.addConstructor builder myCollection

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
