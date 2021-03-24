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
    let myCollection_1 =
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
        (myCollection_1.AsTypeIndex() |> GenericParamOwner.TypeDef)
        (Identifier.ofStr "T")
        ConstraintSet.empty
    |> ignore

    let tparam = TypeSpec.Var 0u |> TypeSpec.create |> addTypeSpec builder
    let tencoded = EncodedType.Var 0u
    let tarray = EncodedType.SZArray(ImmutableArray.Empty, tencoded)

    // val private (*initonly*) items: 'T[]
    let items =
        { Flags = FieldFlags(Private, initOnly = true) |> Flags.instanceField
          FieldName = Identifier.ofStr "items"
          Signature = FieldSignature.create tarray }
        |> ConcreteClass.addInstanceField builder myCollection_1
    // val mutable private index: int32
    let index =
        { Flags = Flags.instanceField(FieldFlags Private)
          FieldName = Identifier.ofStr "index"
          Signature = FieldSignature.create EncodedType.I4 }
        |> ConcreteClass.addInstanceField builder myCollection_1

    let myCollection_1_spec =
        GenericInst.typeDef1 false (myCollection_1.AsTypeIndex()) tencoded
        |> TypeSpec.genericInst
        |> addTypeSpec builder

    let ctor_p_params =
        [|
            EncodedType.SZArray(ImmutableArray.Empty, tencoded) |> ParamItem.create
            ParamItem.create EncodedType.I4
        |]

    let struct (items', _) =
        { MemberRef.MemberName = Identifier.ofStr "items"
          Class = MemberRefParent.TypeSpec myCollection_1_spec
          Signature = FieldSignature.create tarray }
        |> builder.MemberRef.Add
    let struct (index', _) =
        { MemberRef.MemberName = Identifier.ofStr "index"
          Class = MemberRefParent.TypeSpec myCollection_1_spec
          Signature = FieldSignature.create EncodedType.I4 }
        |> builder.MemberRef.Add

    // private new (items: 'T[], index: int32)
    let ctor_p =
        let body content =
            let wr = MethodBodyWriter content
            wr.Ldarg 0us
            wr.Call object_ctor
            // { items = items;
            wr.Ldarg 0us
            wr.Ldarg 1us
            wr.Stfld items'
            // index = 0 }
            wr.Ldarg 0us
            wr.Ldarg 2us
            wr.Stfld index'
            wr.Ret()
            MethodBody.Default
        Constructor (
            body = MethodBody.create ValueNone body,
            implFlags = MethodImplFlags(),
            flags = (ConstructorFlags(Private, true) |> Flags.constructor),
            signature = ObjectConstructorSignature(parameters = ctor_p_params),
            paramList = fun _ i -> Param { Flags = ParamFlags(); ParamName = if i = 0 then "items" else "index" }
        )
        |> ConcreteClass.addConstructor builder myCollection_1

    let struct(ctor_p', _) =
        { MemberRef.MemberName = Identifier.ofStr ".ctor"
          Class = MemberRefParent.TypeSpec myCollection_1_spec
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
            MethodBody.Default
        Constructor (
            body = MethodBody.create ValueNone body,
            implFlags = MethodImplFlags(),
            flags = (ConstructorFlags(Public, true) |> Flags.constructor),
            signature = ObjectConstructorSignature(ParamItem.create EncodedType.I4),
            paramList = fun _ _ -> Param { Flags = ParamFlags(); ParamName = "capacity" }
        )
        |> ConcreteClass.addConstructor builder myCollection_1

    // 'TOther
    let tother_spec =
        TypeSpec.MVar 0u
        |> TypeSpec.create
        |> addTypeSpec builder

    let cast_locals =
        [
            EncodedType.SZArray(ImmutableArray.Empty, EncodedType.MVar 0u) // other: 'TOther[]
            EncodedType.I4 // i: int32
        ]
        |> List.map (LocalVariable.Type ImmutableArray.Empty ImmutableArray.Empty)
        |> ImmutableArray.CreateRange
        |> builder.StandAloneSig.AddLocals
        |> ValueSome

    let struct(ctor_p_other, _) =
        { MemberRef.MemberName = Identifier.ofStr ".ctor"
          Class =
            EncodedType.MVar 0u
            |> GenericInst.typeDef1 false (myCollection_1.AsTypeIndex())
            |> TypeSpec.genericInst
            |> addTypeSpec builder
            |> MemberRefParent.TypeSpec
          Signature =
            let parameters =
                [|
                    EncodedType.SZArray(ImmutableArray.Empty, EncodedType.Var 0u) |> ParamItem.create
                    ParamItem.create EncodedType.I4
                |]
            MethodRefDefaultSignature(true, false, ReturnType.itemVoid, parameters) }
        |> builder.MemberRef.Add

    // member this.Cast<'TOther>(): MyCollection<'TOther when 'TOther :> 'T>
    let cast =
        { Body =
            fun content ->
                let wr = MethodBodyWriter content
                // let other: 'TOther[] = Array.zeroCreate<'TOther> this.index
                wr.Ldarg 0us
                wr.Ldfld index'
                wr.Newarr tother_spec
                wr.Stloc 0us

                // let mutable i: int32 = 0
                wr.Ldc_i4 0
                wr.Stloc 1us

                (*go to condition of loop*)
                let start = wr.Br_s()
                let start_offset = wr.ByteCount

                (*body of the loop*)
                let lbody = Label wr
                // other.[i] <- this.items.[i] :?> 'TOther
                wr.Ldloc 0us
                wr.Ldloc 1us
                wr.Ldarg 0us
                wr.Ldfld items'
                wr.Ldloc 1us
                wr.Ldelem tparam
                wr.Box tparam
                wr.Unbox_any tother_spec
                wr.Stelem tother_spec

                // i <- i + 1
                wr.Ldloc 1us
                wr.Ldc_i4 1
                wr.Add()
                wr.Stloc 1us

                start.SetTarget(int32 (wr.ByteCount - start_offset))
                // i < this.index
                wr.Ldloc 1us
                wr.Ldarg 0us
                wr.Ldfld index'
                (*go to start if true*)
                let go = wr.Blt_s()
                lbody.SetTarget go

                // new MyCollection<TOther>(other, this.index)
                wr.Ldloc 0us
                wr.Ldarg 0us
                wr.Ldfld index'
                wr.Newobj ctor_p_other
                wr.Ret()
                MethodBody(0x4us, true) // NOTE: Don't forget to check that max stack is correct!
            |> MethodBody.create cast_locals
          ImplFlags = MethodImplFlags()
          Flags = InstanceMethodFlags(Public, NoSpecialName, ReuseSlot, hideBySig = true) |> Flags.instanceMethod
          MethodName = Identifier.ofStr "Cast"
          Signature =
            EncodedType.MVar 0u
            |> GenericInst.typeDef1 false (myCollection_1.AsTypeIndex())
            |> EncodedType.GenericInst
            |> ReturnType.encoded
            |> InstanceMethodSignature
          ParamList = ParamList.empty }
        |> ConcreteClass.addInstanceMethod builder myCollection_1

    // 'TOther when 'TOther :> 'T
    GenericParamConstraint.TypeSpec tparam
    |> ConstraintSet.singleton
    |> GenericParam.addNonvariant
        builder
        GenericParamFlags.None
        (cast.AsMethodIndex() |> GenericParamOwner.MethodDef)
        (Identifier.ofStr "TOther")
    |> ignore

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
