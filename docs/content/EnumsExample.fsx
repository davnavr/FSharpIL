(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.EnumsExample

open Expecto
open Swensen.Unquote

open System.IO

open Mono.Cecil

open FSharpIL
#endif
(**
# Enumeration types Example

The following example showcases the generation and usage of enumeration types.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

let example() =
    let builder =
        { Name = Identifier.ofStr "EnumsExample.dll"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    { Name = AssemblyName.ofStr "EnumsExample"
      HashAlgId = ()
      Version = Version(69, 110, 117, 109)
      Flags = ()
      PublicKey = None
      Culture = NullCulture }
    |> Assembly.setRow builder
    |> ignore

    let struct (mscorlib, _) =
        let token =
            PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
            |> builder.Blobs.MiscBytes.GetOrAdd
            |> PublicKeyOrToken
        let row =
            AssemblyRef (
                Version(5, 0, 0, 0),
                AssemblyName.ofStr "System.Runtime",
                token
            )
        AssemblyRef.addRow builder &row

    let object = TypeRef.createReflectedRow builder (ResolutionScope.AssemblyRef mscorlib) typeof<Object>
    let enum = TypeRef.createReflectedRow builder (ResolutionScope.AssemblyRef mscorlib) typeof<Enum>

    (* Generating Enumeration Types *)
    let myenum_values = EnumValueListBuilder(IntegerType.I4, 4)

    List.iter
        (fun (name, value: int32) ->
            let name' = Identifier.ofStr name
            let value' = builder.Blobs.Constant.GetOrAdd(IntegerConstant value)
            myenum_values.TryAdd(name', value') |> ignore)
        [
            "A", 0
            "B", 1
            "C", 256
            "D", -3
            "E", -512
            "F", 12345678
        ]

    // type MyEnum =
    //     | A = 0
    //     | B = 1
    //     | C = 256
    //     | D = -3
    //     | E = -512
    //     | F = 12345678
    let myenum = // TODO: Use safe version for adding enums that uses type lookup.
        let row =
            EnumDef (
                TypeVisibility.Public,
                myenum_values.ToImmutable(),
                Identifier.ofStr "MyEnum",
                ns = "EnumsExample",
                serializable = true
            )
        Unsafe.addEnumRow builder (Extends.TypeRef enum) &row

    (* Using Enumeration Types *)
    // [<AbstractClass; Sealed>] type MyEnumShowcase
    let examples =
        { ClassName = Identifier.ofStr "MyEnumShowcase"
          TypeNamespace = "EnumsExample"
          Access = TypeVisibility.Public
          Flags = ClassFlags() |> Flags.staticClass
          Extends = Extends.TypeRef object }
        |> StaticClass.addRow builder

    // static member Example(): System.Void
    let example =
        let body =
            let locals =
                // value: EnumsExample.MyEnum
                EncodedType.enumDef myenum.Row
                |> LocalVariable.encoded
                |> ImmutableArray.Create
                |> builder.Blobs.LocalVarSig.GetOrAdd
                |> builder.StandAloneSig.AddLocals
                |> ValueSome
            fun content ->
                let wr = MethodBodyWriter content
                // let mutable value = EnumsExample.MyEnum.B
                wr.Ldc_i4 1
                wr.Stloc 0us
                wr.Ret()
                MethodBody.Default
            |> MethodBody.create locals
        StaticMethod (
            body,
            StaticMethodFlags(Public, NoSpecialName, true) |> Flags.staticMethod,
            name = Identifier.ofStr "Example",
            signature = builder.Blobs.MethodDefSig.GetOrAdd(StaticMethodSignature ReturnType.itemVoid)
        )
    StaticMethod.addRow builder (StaticMemberParent.StaticClass examples) &example |> ignore

    CliMetadata builder |> PEFile.ofMetadata ImageFileFlags.dll

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    let example' = lazy example()
    let metadata = lazy PEFile.toCecilModule example'.Value

    afterRunTests <| fun() -> if metadata.IsValueCreated then metadata.Value.Dispose()

    testList "enums example" [
        testCase "can save to disk" <| fun() ->
            let path = Path.Combine(__SOURCE_DIRECTORY__, "exout", "EnumsExample.dll")
            WritePE.toPath path example'.Value

        testCase "enum has correct field values" <| fun() ->
            let expected = List.map Some [ 0; 1; 256; -3; -512; 12345678 ]
            let actual =
                metadata.Value.GetType("EnumsExample", "MyEnum").Fields
                |> Seq.where (fun field -> field.Attributes.HasFlag FieldAttributes.Static)
                |> Seq.map (fun field -> Option.ofObj field.Constant |> Option.map unbox<int32>)
                |> List.ofSeq
            expected =! actual
    ]
#endif
