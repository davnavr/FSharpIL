(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.DelegatesExample

open Expecto

open Swensen.Unquote

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
open FSharpIL.Metadata.Unchecked
open FSharpIL.Metadata.UncheckedExn
open FSharpIL.Metadata.CliMetadata
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
    let str =
        { TypeName = Identifier.ofStr "String"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder
    let stringb =
        { TypeName = Identifier.ofStr "StringBuilder"
          TypeNamespace = "System.Text"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder
    let mcdelegate =
        { TypeName = Identifier.ofStr "MulticastDelegate"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder
    let aresult =
        { TypeName = Identifier.ofStr "IAsyncResult"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder
    let acallback =
        { TypeName = Identifier.ofStr "AsyncCallback"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder
    let func_2 =
        { TypeName = Identifier.ofStr "Func`2"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder

    let struct (str_length, _) =
        { Class = MemberRefParent.TypeRef str
          MemberName = Identifier.ofStr "get_Length"
          // member _.get_Length(): string
          Signature = MethodRefDefaultSignature(true, false, ReturnType.itemI4) }
        |> referenceDefaultMethod builder
    let struct (str_indexof, _) =
        { Class = MemberRefParent.TypeRef str
          MemberName = Identifier.ofStr "IndexOf"
          // member _.IndexOf(value: string): int
          Signature = MethodRefDefaultSignature(true, false, ReturnType.itemI4, ParamItem.create EncodedType.String) }
        |> referenceDefaultMethod builder
    let struct (stringb_ctor, _) =
        { Class = MemberRefParent.TypeRef stringb
          MemberName = Identifier.ofStr ".ctor"
          // new (capacity: int32)
          Signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid, ParamItem.create EncodedType.I4) }
        |> referenceDefaultMethod builder
    let struct (stringb_tostring, _) =
        { Class = MemberRefParent.TypeRef stringb
          MemberName = Identifier.ofStr "ToString"
          // override _.ToString(): string
          Signature = MethodRefDefaultSignature(true, false, ReturnType.encoded EncodedType.String) }
        |> referenceDefaultMethod builder
    let struct (stringb_append, _) =
        { Class = MemberRefParent.TypeRef stringb
          MemberName = Identifier.ofStr "Append"
          Signature =
            // member _.Append(value: string): System.StringBuilder
            MethodRefDefaultSignature (
                true,
                false,
                EncodedType.typeRefClass stringb |> ReturnType.encoded,
                ParamItem.create EncodedType.String
            ) }
        |> referenceDefaultMethod builder

    (*Generating Delegate Types*)

    // type MyDelegate = delegate of (string * int32) -> string
    let mydel =
        let parameters = Array.map ParamItem.create [| EncodedType.String; EncodedType.I4 |]
        UncheckedExn.Unsafe.AddDelegate (
            builder,
            mcdelegate,
            EncodedType.Class(TypeDefOrRefOrSpecEncoded.TypeRef aresult),
            EncodedType.Class(TypeDefOrRefOrSpecEncoded.TypeRef acallback),
            DelegateDef (
                TypeVisibility.Public,
                ReturnType.encoded EncodedType.String,
                ImmutableArray.CreateRange parameters,
                Identifier.ofStr "MyDelegate"
            )
        )
    let mydel_encoded =
        mydel.Row.AsTypeIndex()
        |> TypeDefOrRefOrSpecEncoded.TypeDef
        |> EncodedType.Class
 
    (*Using Delegate Types*)

    // type MyClass
    let myclass =
        { Access = TypeVisibility.Public
          Flags = ClassFlags(AutoLayout, AnsiClass, beforeFieldInit = true) |> Flags.concreteClass
          ClassName = Identifier.ofStr "MyClass"
          TypeNamespace = String.Empty
          Extends = Extends.TypeRef object }
        |> ConcreteClass.addTypeDef builder

    // static member DuplicateString(str: string, times: int32): string
    let dupstr =
        let body =
            let locals =
                // sb: System.StringBuilder
                EncodedType.typeRefClass stringb
                |> LocalVariable.encoded
                |> ImmutableArray.Create
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
        StaticMethod (
            body,
            StaticMethodFlags(Public, NoSpecialName, true) |> Flags.staticMethod,
            Identifier.ofStr "DuplicateString",
            signature,
            fun _ i -> Param { Flags = ParamFlags(); ParamName = if i = 0 then "str" else "times" }
        )
        |> ConcreteClass.addStaticMethod builder myclass

    // static member Example1(): string
    let example1_body =
        let locals =
            LocalVariable.encoded mydel_encoded
            |> ImmutableArray.Create
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
        example1_body,
        StaticMethodFlags(Public, NoSpecialName, true) |> Flags.staticMethod,
        name = Identifier.ofStr "Example1",
        signature = StaticMethodSignature(ReturnType.encoded EncodedType.String)
    )
    |> ConcreteClass.addStaticMethod builder myclass
    |> ignore

    // System.Func<string, int32>
    let func_2_inst = GenericInst(TypeDefOrRefOrSpecEncoded.TypeRef func_2, false, EncodedType.String, EncodedType.I4)
    // new(_: object, _: System.IntPtr)
    let struct(func_2_inst_ctor, _) =
        { MemberRef.MemberName = Identifier.ofStr ".ctor"
          Class =
            func_2_inst
            |> TypeSpec.genericInst
            |> addTypeSpec builder
            |> MemberRefParent.TypeSpec
          Signature =
            let parameters = Array.map ParamItem.create [| EncodedType.Object; EncodedType.I |]
            MethodRefDefaultSignature(true, false, ReturnType.itemVoid, parameters) }
        |> builder.MemberRef.Add

    // static member Example2(str: string): System.Func<string, int32>
    let example2_body content =
        let wr = MethodBodyWriter content
        wr.Ldarg 0us
        wr.Ldftn str_indexof
        wr.Newobj func_2_inst_ctor
        wr.Ret()
        MethodBody()
    StaticMethod (
        MethodBody.create ValueNone example2_body,
        StaticMethodFlags(Public, NoSpecialName, true) |> Flags.staticMethod,
        Identifier.ofStr "Example2",
        StaticMethodSignature(EncodedType.GenericInst func_2_inst |> ReturnType.encoded, ParamItem.create EncodedType.String),
        Param { ParamName = "str"; Flags = ParamFlags() } |> ParamList.singleton
    )
    |> ConcreteClass.addStaticMethod builder myclass
    |> ignore

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
