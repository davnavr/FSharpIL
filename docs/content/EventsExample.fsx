(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.EventsExample
#if !BENCHMARK
open Expecto

open Swensen.Unquote

open System.IO

open FSharpIL
#endif
#endif
(**
# Events Example

The following example showcases the generation of events.

Because F# provides additional classes and attributes used when creating events, the corresponding decompiled IL code is shown in
C# instead.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

let example() =
    let ns = "FakeGuiLibrary.Controls"
    let builder =
        { Name = sprintf "%s.dll" ns |> Identifier.ofStr
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    let assembly =
        { Name = AssemblyName.ofStr ns
          HashAlgId = ()
          Version = Version(1, 0, 0, 0)
          Flags = ()
          PublicKey = None
          Culture = NullCulture }
        |> Assembly.setRow builder

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
    let dele = TypeRef.createReflectedRow builder mscorlib' typeof<Delegate>
    let tfm = TypeRef.createReflectedRow builder mscorlib' typeof<System.Runtime.Versioning.TargetFrameworkAttribute>
    let evhandler = TypeRef.createReflectedRow builder mscorlib' typeof<EventHandler>

    let dele' = EncodedType.typeRefClass dele
    let evhandler' = EncodedType.typeRefClass evhandler

    let delemodify name =
        let parameters = ImmutableArray.Create(ParamItem.create dele', ParamItem.create dele')
        let signature = MethodRefDefaultSignature(false, false, ReturnType.encoded dele', parameters)
        { Class = MemberRefParent.TypeRef dele
          MemberName = Identifier.ofStr name
          Signature = builder.Blobs.MethodRefSig.GetOrAdd signature }
        |> MethodRef.addRowDefault builder

    let struct(combine, _) = delemodify "Combine"
    let struct(remove, _) = delemodify "Remove"

    // TargetFrameworkAttribute(_: string)
    let struct(tfm_ctor, _) =
        { Class = MemberRefParent.TypeRef tfm
          MemberName = Identifier.ofStr ".ctor"
          Signature =
            let parameters = ImmutableArray.Create(ParamItem.create EncodedType.String)
            MethodRefDefaultSignature(true, false, ReturnType.itemVoid, parameters)
            |> builder.Blobs.MethodRefSig.GetOrAdd }
        |> MethodRef.addRowDefault builder

    Assembly.setTargetFramework builder assembly tfm_ctor ".NETCoreApp,Version=v5.0"

    // public class Button
    let button =
        { Access = TypeVisibility.Public
          Flags = ClassFlags(AutoLayout, AnsiClass, beforeFieldInit = true) |> Flags.concreteClass
          ClassName = Identifier.ofStr "Button"
          TypeNamespace = ns
          Extends = Extends.TypeRef object }
        |> ConcreteClass.addRow builder
    let button' = InstanceMemberOwner.ConcreteClass button

    (* Creating Events *)
    let addBody (field: RawIndex<InstanceField>) (etype: EventType) =
        fun content ->
            let wr = MethodBodyWriter content
            wr.Ldarg 0us
            wr.Dup()
            wr.Ldfld field
            wr.Ldarg 1us
            wr.Call combine
            wr.Castclass etype
            wr.Stfld field
            wr.Ret()
            MethodBody 3us
        |> MethodBody.create ValueNone

    let removeBody (field: RawIndex<InstanceField>) (etype: EventType) =
        fun content ->
            let wr = MethodBodyWriter content
            wr.Ldarg 0us
            wr.Dup()
            wr.Ldfld field
            wr.Ldarg 1us
            wr.Call remove
            wr.Castclass etype
            wr.Stfld field
            wr.Ret()
            MethodBody()
        |> MethodBody.create ValueNone

    let click = Identifier.ofStr "Click"
    // private System.EventHandler Click;
    let fclick =
        { FieldName = click
          Flags = FieldFlags(Private) |> Flags.instanceField
          Signature = builder.Blobs.FieldSig.GetOrAdd(FieldSignature.create evhandler') }
        |> InstanceField.addRow builder button'

    // event System.EventHandler Click;
    let eclick =
        Event.createInstance
            builder
            button'
            { EventName = click
              EventType = EventType.TypeRef evhandler
              Flags = NoSpecialName }
            Public
            VTableLayout.NewSlot
            true
            false
            (addBody fclick)
            (removeBody fclick)

    (* Events With Data*)

    (* Using Custom Event Handlers *)

    CliMetadata builder |> PEFile.ofMetadata ImageFileFlags.dll

(*** hide ***)
#if COMPILED && !BENCHMARK
[<Tests>]
let tests =
    let example' = lazy example()

    testList "events example" [
        testCase "can save to disk" <| fun() ->
            let path = Path.Combine(__SOURCE_DIRECTORY__, "exout", "EventsExample.dll")
            WritePE.toPath path example'.Value
    ]
#endif
