(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.DelegatesExample

open Expecto

open System.IO

open FSharpIL
#endif
(**
# Delegates Example

The following example showcases the generation and usage of delegate types.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

let example() =
    let builder =
        { Name = Identifier.ofStr "DelegatesExample.dll"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    { Name = AssemblyName.ofStr "DelegatesExample"
      HashAlgId = ()
      Version = Version(68, 101, 108, 101)
      Flags = ()
      PublicKey = None
      Culture = NullCulture }
    |> Assembly.setRow builder
    |> ignore

    let struct (mscorlib, _) =
        let token = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
        let row =
            AssemblyRef (
                Version(5, 0, 0, 0),
                AssemblyName.ofStr "System.Runtime",
                PublicKeyOrToken(builder.Blobs.MiscBytes.GetOrAdd token)
            )
        AssemblyRef.addRow builder &row

    let object = TypeRef.createReflectedRow builder (ResolutionScope.AssemblyRef mscorlib) typeof<Object>
    let str = TypeRef.createReflectedRow builder (ResolutionScope.AssemblyRef mscorlib) typeof<String>
    let mcdelegate = TypeRef.createReflectedRow builder (ResolutionScope.AssemblyRef mscorlib) typeof<MulticastDelegate>
    let aresult = TypeRef.createReflectedRow builder (ResolutionScope.AssemblyRef mscorlib) typeof<IAsyncResult>
    let acallback = TypeRef.createReflectedRow builder (ResolutionScope.AssemblyRef mscorlib) typeof<AsyncCallback>
    let stringb = TypeRef.createReflectedRow builder (ResolutionScope.AssemblyRef mscorlib) typeof<System.Text.StringBuilder>
    let func_2 = TypeRef.createReflectedRow builder (ResolutionScope.AssemblyRef mscorlib) typedefof<Func<_, _>>

    // member _.get_Length(): string
    let struct (str_length, _) =
        let signature = MethodRefDefaultSignature(true, false, ReturnType.itemI4)
        let row =
            { Class = MemberRefParent.TypeRef str
              MemberName = Identifier.ofStr "get_Length"
              Signature = builder.Blobs.MethodRefSig.GetOrAdd signature }
        MethodRef.addRowDefault builder &row
    // member _.IndexOf(_: string): int
    let struct (str_indexof, _) =
        let signature = MethodRefDefaultSignature(true, false, ReturnType.itemI4, ParamItem.create EncodedType.String)
        let row =
            { Class = MemberRefParent.TypeRef str
              MemberName = Identifier.ofStr "IndexOf"
              Signature = builder.Blobs.MethodRefSig.GetOrAdd signature }
        MethodRef.addRowDefault builder &row
    // new(_: int32)
    let struct (stringb_ctor, _) =
        let signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid, ParamItem.create EncodedType.I4)
        let row =
            { Class = MemberRefParent.TypeRef stringb
              MemberName = Identifier.ofStr ".ctor"
              Signature = builder.Blobs.MethodRefSig.GetOrAdd signature }
        MethodRef.addRowDefault builder &row
    // override _.ToString(): string
    let struct (stringb_tostring, _) =
        let signature = MethodRefDefaultSignature(true, false, ReturnType.encoded EncodedType.String)
        let row =
            { Class = MemberRefParent.TypeRef stringb
              MemberName = Identifier.ofStr "ToString"
              Signature = builder.Blobs.MethodRefSig.GetOrAdd signature }
        MethodRef.addRowDefault builder &row
    // member _.Append(_: string): System.StringBuilder
    let struct (stringb_append, _) =
        let signature =
            MethodRefDefaultSignature (
                true,
                false,
                EncodedType.typeRefClass stringb |> ReturnType.encoded,
                ParamItem.create EncodedType.String
            )
        let row =
            { Class = MemberRefParent.TypeRef stringb
              MemberName = Identifier.ofStr "Append"
              Signature = builder.Blobs.MethodRefSig.GetOrAdd signature }
        MethodRef.addRowDefault builder &row

    (* Generating Delegate Types *)

    // type MyDelegate = delegate of (string * int32) -> string
    let mydel = // TODO: For this example, use the safe version of function to create a delegate type thta uses type lookup.
        let parameters = Array.map ParamItem.create [| EncodedType.String; EncodedType.I4 |]
        let def =
            DelegateDef (
                TypeVisibility.Public,
                ReturnType.encoded EncodedType.String,
                ImmutableArray.Create(ParamItem.create EncodedType.String, ParamItem.create EncodedType.I4),
                Identifier.ofStr "MyDelegate"
            )
        Unsafe.addDelegateRow
            builder
            (Extends.TypeRef mcdelegate)
            (EncodedType.Class(TypeDefOrRefOrSpecEncoded.TypeRef aresult))
            (EncodedType.Class(TypeDefOrRefOrSpecEncoded.TypeRef acallback))
            &def
    let mydel_encoded =
        mydel.Row.AsTypeIndex()
        |> TypeDefOrRefOrSpecEncoded.TypeDef
        |> EncodedType.Class
 
    (* Using Delegate Types *)

    // type MyClass
    let myclass =
        { Access = TypeVisibility.Public
          Flags = ClassFlags(AutoLayout, AnsiClass, beforeFieldInit = true) |> Flags.concreteClass
          ClassName = Identifier.ofStr "MyClass"
          TypeNamespace = String.Empty
          Extends = Extends.TypeRef object }
        |> ConcreteClass.addRow builder

    // static member DuplicateString(str: string, times: int32): string
    let dupstr =
        let body =
            let locals =
                // sb: System.StringBuilder
                EncodedType.typeRefClass stringb
                |> LocalVariable.encoded
                |> ImmutableArray.Create
                |> builder.Blobs.LocalVarSig.GetOrAdd
                |> builder.StandAloneSig.AddLocals
                |> ValueSome
            fun content ->
                let wr = MethodBodyWriter content
                // let sb: System.StringBuilder = new System.StringBuilder(str.Length * times)
                wr.Ldarg 0us
                wr.Callvirt str_length
                wr.Ldarg 1us
                wr.Mul()
                wr.Newobj stringb_ctor
                wr.Stloc 0us
                let start = wr.Br_s()
                let body = Label wr

                // sb.Append(str) |> ignore
                wr.Ldloc 0us
                wr.Ldarg 0us
                wr.Call stringb_append
                wr.Pop()

                // times <- times - 1
                wr.Ldarg 1us
                wr.Ldc_i4 1
                wr.Sub()
                wr.Starg 1us

                Label(wr).SetTarget start
                // times > 0
                wr.Ldarg 1us
                wr.Ldc_i4 0
                body.SetTarget(wr.Bgt_s())

                // sb.ToString()
                wr.Ldloc 0us
                wr.Callvirt stringb_tostring
                wr.Ret()
                MethodBody(2us, true)
            |> MethodBody.create locals
        let signature =
            let parameters = Array.map ParamItem.create [| EncodedType.String; EncodedType.I4 |]
            StaticMethodSignature(ReturnType.encoded EncodedType.String, parameters)
        let method =
            StaticMethod (
                body,
                StaticMethodFlags(Public, NoSpecialName, true) |> Flags.staticMethod,
                Identifier.ofStr "DuplicateString",
                builder.Blobs.MethodDefSig.GetOrAdd signature,
                fun _ i -> Param { Flags = ParamFlags(); ParamName = if i = 0 then "str" else "times" }
            )
        StaticMethod.addRow builder (StaticMemberParent.ConcreteClass myclass) &method

    // static member Example1(): string
    let example1 =
        let body =
            let locals =
                LocalVariable.encoded mydel_encoded
                |> ImmutableArray.Create
                |> builder.Blobs.LocalVarSig.GetOrAdd
                |> builder.StandAloneSig.AddLocals
                |> ValueSome
            fun content ->
                let wr = MethodBodyWriter content
                // let del = new MyDelegate(fun arg1 arg2 -> MyClass.DuplicateString(arg1, arg2))
                wr.Ldnull()
                wr.Ldftn dupstr
                wr.Newobj mydel.Constructor
                wr.Stloc 0us

                // del.Invoke("Test", 4)
                wr.Ldloc 0us
                wr.Ldstr "Test"
                wr.Ldc_i4 4
                wr.Callvirt mydel.Invoke
                wr.Ret()
                MethodBody(3us, true)
            |> MethodBody.create locals
        StaticMethod (
            body,
            StaticMethodFlags(Public, NoSpecialName, true) |> Flags.staticMethod,
            name = Identifier.ofStr "Example1",
            signature = builder.Blobs.MethodDefSig.GetOrAdd(StaticMethodSignature(ReturnType.encoded EncodedType.String))
        )
    StaticMethod.addRow builder (StaticMemberParent.ConcreteClass myclass) &example1 |> ignore

    // System.Func<string, int32>
    let func_2_inst = GenericInst(TypeDefOrRefOrSpecEncoded.TypeRef func_2, false, EncodedType.String, EncodedType.I4)
    // new (_: object, _: System.IntPtr)
    let struct(func_2_inst_ctor, _) =
        let method =
            { MemberRef.MemberName = Identifier.ofStr ".ctor"
              Class =
                TypeSpec.GenericInst func_2_inst
                |> TypeSpec.createRow builder
                |> MemberRefParent.TypeSpec
              Signature =
                let parameters = Array.map ParamItem.create [| EncodedType.Object; EncodedType.I |]
                let signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid, parameters)
                builder.Blobs.MethodRefSig.GetOrAdd signature }
        MethodRef.addRowDefault builder &method

    // static member Example2(str: string): System.Func<string, int32>
    let example2 =
        let body content =
            let wr = MethodBodyWriter content
            wr.Ldarg 0us
            wr.Ldftn str_indexof
            wr.Newobj func_2_inst_ctor
            wr.Ret()
            MethodBody()
        let signature =
            StaticMethodSignature(EncodedType.GenericInst func_2_inst |> ReturnType.encoded, ParamItem.create EncodedType.String)
        StaticMethod (
            MethodBody.create ValueNone body,
            StaticMethodFlags(Public, NoSpecialName, true) |> Flags.staticMethod,
            Identifier.ofStr "Example2",
            builder.Blobs.MethodDefSig.GetOrAdd signature,
            Param { ParamName = "str"; Flags = ParamFlags() } |> ParamList.singleton
        )
    StaticMethod.addRow builder (StaticMemberParent.ConcreteClass myclass) &example2 |> ignore

    CliMetadata builder |> PEFile.ofMetadata ImageFileFlags.dll

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    let example' = lazy example()

    testList "delegates example" [
        testCase "can save to disk" <| fun() ->
            let path = Path.Combine(__SOURCE_DIRECTORY__, "exout", "DelegatesExample.dll")
            WritePE.toPath path example'.Value
    ]
#endif
