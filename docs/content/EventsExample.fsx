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
    let tfm = TypeRef.createReflectedRow builder mscorlib' typeof<System.Runtime.Versioning.TargetFrameworkAttribute>
    let evhandler = TypeRef.createReflectedRow builder mscorlib' typeof<EventHandler>

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
    let button' = ConcreteClass.typeIndex button

    (* Creating Events *)
    // event System.EventHandler Click;
    let click =
        //let add

        //let remove

        { EventName = Identifier.ofStr "Click"
          EventType = EventType.TypeRef evhandler
          Flags = NoSpecialName }
        Event.tryCreateInstanceRow
            builder
            (InstanceMemberOwner.ConcreteClass button)

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
