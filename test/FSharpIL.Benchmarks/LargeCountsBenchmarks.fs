namespace FSharpIL

#if BENCHMARK
open BenchmarkDotNet.Attributes
#else
open Expecto
#endif

open System
open System.IO
open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

[<RequireQualifiedAccess>]
module LargeCountsBenchmarks =
    let generate (size: int32) =
        let mname = sprintf "LargeCounts%i" size

        let builder =
            { Name = sprintf "%s.dll" mname |> Identifier.ofStr
              Mvid = Guid.NewGuid() }
            |> CliMetadataBuilder

        { Name = AssemblyName.ofStr mname
          HashAlgId = ()
          Version = Version()
          Flags = ()
          PublicKey = None
          Culture = NullCulture }
        |> Assembly.setRow builder
        |> ignore

        let struct (mscorlib, _) = AssemblyRef.addReflectedRow builder typeof<Object>.Assembly
        let mscorlib' = ResolutionScope.AssemblyRef mscorlib

        let object = TypeRef.createReflectedRow builder mscorlib' typeof<Object>
        let mcdelegate = TypeRef.createReflectedRow builder mscorlib' typeof<MulticastDelegate>
        let asyncResult = TypeRef.createReflectedRow builder mscorlib' typeof<IAsyncResult>
        let asyncCallback = TypeRef.createReflectedRow builder mscorlib' typeof<AsyncCallback>

        let assemblyRefs =
            Array.init size <| fun i ->
                let struct (assem, _) =
                    AssemblyRef.createRow
                        builder
                        (Version(i, i, i, i))
                        (sprintf "Assembly%i" i |> AssemblyName.ofStr)
                        PublicKeyOrToken.NoPublicKey
                        AssemblyCulture.NullCulture
                        ValueNone
                assem

        let typeRefs =
            Array.init size <| fun i ->
                TypeRef.createRow
                    builder
                    (ResolutionScope.AssemblyRef assemblyRefs.[i])
                    (sprintf "TypeRef%i" i |> Identifier.ofStr)
                    (sprintf "Namespace%i" i)

        let typeDefCount = size / 5

        let classes =
            Array.init typeDefCount <| fun i ->
                // TODO: Figure out how to add other kinds of classes.
                let flags = ClassFlags(LayoutFlag.AutoLayout, StringFormattingFlag.AnsiClass, beforeFieldInit = true)
                { Access = TypeVisibility.Public
                  Flags = Flags.concreteClass flags
                  ClassName = sprintf "Class%i" i |> Identifier.ofStr
                  TypeNamespace = sprintf "Namespace%i" i
                  Extends = Extends.TypeRef object } // TODO: Get System.Object
                |> ConcreteClass.addRow builder

        //let structs

        let delegates =
            Array.init typeDefCount <| fun i ->
                // TODO: Use different number of parameters for delegates.
                let row =
                    DelegateDef (
                        TypeVisibility.Public,
                        ReturnType.encoded EncodedType.I4,
                        ImmutableArray.Create(ParamItem.create EncodedType.String |> DelegateParam),
                        sprintf "Delegate%i" i |> Identifier.ofStr,
                        sprintf "Namespace%i" i
                    )
                Unsafe.addDelegateRow
                    builder
                    (Extends.TypeRef mcdelegate)
                    (EncodedType.typeRefClass asyncResult)
                    (EncodedType.typeRefClass asyncCallback)
                    &row

        //let enums

        //let interfaces

        let files =
            Array.init size <| fun i ->
                let bytes = byte i |> Array.create 0xF
                let hash = ImmutableArray.Create(items = bytes) |> builder.Blobs.MiscBytes.GetOrAdd
                let struct(file, _) = File.createRow builder false hash (sprintf "File%i" i |> Identifier.ofStr)
                file

        let ifields =
            let rows = List(classes.Length)
            // TODO: Have more fields for each type
            for tdef in classes do
                let ftype = TypeDefOrRefOrSpecEncoded.TypeDef(ConcreteClass.typeIndex tdef) |> EncodedType.Class
                { FieldName = Identifier.ofStr "myInstanceField"
                  Flags = FieldFlags Private
                  Signature = FieldSignature.create ftype |> builder.Blobs.FieldSig.GetOrAdd }
                |> InstanceField.addRow builder (InstanceMemberOwner.ConcreteClass tdef)
                |> rows.Add
            rows

        CliMetadata builder |> PEFile.ofMetadata ImageFileFlags.exe
#if !BENCHMARK
    [<Tests>]
    let tests =
        List.map
            (fun size ->
                testCase (sprintf "can save %i to disk" size) <| fun() ->
                    let path =
                        Path.Combine (
                            __SOURCE_DIRECTORY__,
                            "..",
                            "..",
                            "docs",
                            "content",
                            "exout",
                            sprintf "LargeCounts%i.dll" size
                        )
                    generate size |> WritePE.toPath path)
            [ 100; 1000 ]
        |> testList "large counts"
#endif

#if BENCHMARK
[<MemoryDiagnoser>]
type LargeCountsBenchmarks () =
    [<Benchmark; Arguments(100, 1000, 10000)>]
    member _.Generate size = LargeCountsBenchmarks.generate size
#endif
