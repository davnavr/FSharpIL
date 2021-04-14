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
    let evhandler_1 = TypeRef.createReflectedRow builder mscorlib' typedefof<EventHandler<_>>
    let evargs = TypeRef.createReflectedRow builder mscorlib' typeof<EventArgs>

    let dele' = EncodedType.typeRefClass dele
    let evhandler' = EncodedType.typeRefClass evhandler
    let evargs' = EncodedType.typeRefClass evargs

    let delemodify =
        let parameters = ImmutableArray.Create(ParamItem.create dele', ParamItem.create dele')
        let signature = MethodRefDefaultSignature(false, false, ReturnType.encoded dele', parameters)
        fun name ->
            { Class = MemberRefParent.TypeRef dele
              MemberName = Identifier.ofStr name
              Signature = builder.Blobs.MethodRefSig.GetOrAdd signature }
            |> MethodRef.addRowDefault builder

    // static System.Delegate Combine(System.Delegate a, System.Delegate b)
    let struct(combine, _) = delemodify "Combine"
    // static System.Delegate Remove(System.Delegate source, System.Delegate value)
    let struct(remove, _) = delemodify "Remove"

    // void Invoke(object sender, System.EventArgs e)
    let struct(evhandler_invoke, _) =
        let parameters = ImmutableArray.Create(ParamItem.create EncodedType.Object, ParamItem.create evargs')
        let signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid, parameters)
        { Class = MemberRefParent.TypeRef evhandler
          MemberName = Identifier.ofStr "Invoke"
          Signature = builder.Blobs.MethodRefSig.GetOrAdd signature }
        |> MethodRef.addRowDefault builder

    // static System.EventArgs Empty;
    let struct(evargs_empty, _) =
        { Class = MemberRefParent.TypeRef evargs
          MemberName = Identifier.ofStr "Empty"
          Signature = builder.Blobs.FieldSig.GetOrAdd(FieldSignature.create evargs') }
        |> FieldRef.addRow builder

    // EventArgs()
    let struct(evargs_ctor, _) =
        let signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid, ImmutableArray.Empty)
        { Class = MemberRefParent.TypeRef evargs
          MemberName = Identifier.ofStr ".ctor"
          Signature = builder.Blobs.MethodRefSig.GetOrAdd signature }
        |> MethodRef.addRowDefault builder

    // TargetFrameworkAttribute(string frameworkName)
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
            MethodBody 3us
        |> MethodBody.create ValueNone

    let click = Identifier.ofStr "Click"
    // private System.EventHandler Click;
    let fclick =
        { FieldName = click
          Flags = FieldFlags Private |> Flags.instanceField
          Signature = builder.Blobs.FieldSig.GetOrAdd(FieldSignature.create evhandler') }
        |> InstanceField.addRow builder button'

    // public event System.EventHandler Click;
    let eclick =
        Event.createInstance
            builder
            button'
            { EventName = click
              EventType = EventType.Ref evhandler
              Flags = NoSpecialName }
            Public
            VTableLayout.NewSlot
            true
            false
            (addBody fclick) // this.Click = (System.EventHandler)System.Delegate.Combine(this.click, value);
            (removeBody fclick) // this.Click = (System.EventHandler)System.Delegate.Remove(this.click, value);

    (* Raising custom events *)
    let mouseclick_body =
        let locals =
            // System.EventHandler click;
            LocalVariable.encoded evhandler'
            |> ImmutableArray.Create
            |> builder.Blobs.LocalVarSig.GetOrAdd
            |> builder.StandAloneSig.AddLocals
            |> ValueSome
        fun content ->
            let wr = MethodBodyWriter content
            // click = this.Click;
            wr.Ldarg 0us
            wr.Ldfld fclick
            wr.Stloc 0us

            // click.Invoke(this, System.EventArgs.Empty);
            wr.Ldloc 0us
            wr.Ldarg 0us
            wr.Ldsfld evargs_empty
            wr.Callvirt evhandler_invoke

            wr.Ret()
            MethodBody(2us, true)
        |> MethodBody.create locals

    // public void MouseClick()
    InstanceMethod (
        mouseclick_body,
        InstanceMethodFlags(Public, NoSpecialName, ReuseSlot, hideBySig = true) |> Flags.instanceMethod,
        Identifier.ofStr "MouseClick",
        builder.Blobs.MethodDefSig.GetOrAdd(InstanceMethodSignature ReturnType.itemVoid)
    )
    |> InstanceMethod.addRow builder button'
    |> ignore

    (* Adding and removing listeners *)
    let addandremove_body =
        ()

    (* Events with data*)
    let rename = Identifier.ofStr "Rename"
    // public class RenameEventArgs : System.EventArgs
    let renameargs =
        { Access = TypeVisibility.Public
          Flags = ClassFlags(AutoLayout, AnsiClass, beforeFieldInit = true) |> Flags.concreteClass
          ClassName = Identifier.ofStr "EventArgs" |> Identifier.concat2 rename
          TypeNamespace = ns
          Extends = Extends.TypeRef evargs }
        |> ConcreteClass.addRow builder
    let renameargs' = InstanceMemberOwner.ConcreteClass renameargs

    // public readonly string Name;
    let rename_name =
        let signature = FieldSignature.create EncodedType.String
        { FieldName = Identifier.ofStr "Name"
          Flags = FieldFlags(Public, initOnly = true) |> Flags.instanceField
          Signature = builder.Blobs.FieldSig.GetOrAdd signature }
        |> InstanceField.addRow builder renameargs'

    // public RenameEventArgs(string name)
    let rename_ctor =
        let body =
            fun content ->
                let wr = MethodBodyWriter content
                // : base()
                wr.Ldarg 0us
                wr.Call evargs_ctor

                // this.Name <- name;
                wr.Ldarg 0us
                wr.Ldarg 1us
                wr.Stfld rename_name

                wr.Ret()
                MethodBody 2us
            |> MethodBody.create ValueNone
        ObjectConstructor (
            body,
            MethodImplFlags(),
            ConstructorFlags(Public, true) |> Flags.constructor,
            ObjectConstructorSignature(ParamItem.create EncodedType.String) |> builder.Blobs.MethodDefSig.GetOrAdd,
            ParamList.named [| "name" |]
        )
        |> ObjectConstructor.addRow builder renameargs'

    // System.EventHandler<FakeGuiLibrary.Controls.RenameEventArgs>
    let evhandler_rename = GenericInst(TypeDefOrRefOrSpecEncoded.TypeRef evhandler_1, ImmutableArray.Create EncodedType.String)

    // TODO: Fix bug, since FieldRows one owner at a time, adding with a different owner later will mean previous field indices will be invalid.
    // private System.EventHandler<FakeGuiLibrary.Controls.RenameEventArgs> Rename;
    let frename =
        let signature = EncodedType.GenericInst evhandler_rename |> FieldSignature.create
        { FieldName = rename
          Flags = FieldFlags Private |> Flags.instanceField
          Signature = builder.Blobs.FieldSig.GetOrAdd signature }
        |> InstanceField.addRow builder button'

    // public event System.EventHandler<FakeGuiLibrary.Controls.RenameEventArgs> Rename;
    Event.createInstance
        builder
        button'
        { EventName = rename
          EventType = TypeSpec.GenericInst evhandler_rename |> TypeSpec.createRow builder |> EventType.Spec // TODO: Fix, apparantly this TypeSpec is invalid for EventType
          Flags = NoSpecialName }
        Public
        VTableLayout.NewSlot
        true
        false
        (addBody frename)
        (removeBody frename)
    |> ignore<GeneratedEvent>

    (* Using custom event handlers *)

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
