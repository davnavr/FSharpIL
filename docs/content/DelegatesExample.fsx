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
        { Name = Identifier.ofStr "DelegatesExample.netmodule"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

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

    let struct (str_length, _) =
        { Class = MemberRefParent.TypeRef str
          MemberName = Identifier.ofStr "get_Length"
          // member _.get_Length(): string
          Signature = MethodRefDefaultSignature(true, false, ReturnType.itemI4) }
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
        let locals =
            // sb: System.StringBuilder
            EncodedType.typeRefClass stringb
            |> LocalVariable.encoded
            |> ImmutableArray.Create
            |> builder.StandAloneSig.AddLocals
            |> ValueSome
        { MethodName = Identifier.ofStr "DuplicateString"
          ImplFlags = MethodImplFlags()
          Flags = StaticMethodFlags(Public, NoSpecialName, true) |> Flags.staticMethod
          ParamList = fun _ i -> Param { Flags = ParamFlags(); ParamName = if i = 0 then "str" else "times" }
          Signature =
            let parameters = Array.map ParamItem.create [| EncodedType.String; EncodedType.I4 |]
            StaticMethodSignature(ReturnType.encoded EncodedType.String, parameters)
          Body =
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
            |> MethodBody.create locals }
        |> ConcreteClass.addStaticMethod builder myclass

    // member _.
    // TODO: Add instance method to showcase creating delegates from objects.

    // static member Example1(): MyDelegate
    { MethodName = Identifier.ofStr "Example1"
      ImplFlags = MethodImplFlags()
      Flags = StaticMethodFlags(Public, NoSpecialName, true) |> Flags.staticMethod
      ParamList = ParamList.empty
      Signature = ReturnType.encoded EncodedType.String |> StaticMethodSignature
      Body =
        let locals =
            LocalVariable.encoded mydel_encoded
            |> ImmutableArray.Create
            |> builder.StandAloneSig.AddLocals
            |> ValueSome
        fun content ->
            let wr = MethodBodyWriter content
            // let del = new MyDelegate(MyClass.DuplicateString)
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
            MethodBody(2us, true)
        |> MethodBody.create locals }
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
            let path = Path.Combine(__SOURCE_DIRECTORY__, "exout", "DelegatesExample.netmodule")
            WritePE.toPath path example'.Value
    ]
#endif
