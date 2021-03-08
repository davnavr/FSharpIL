(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.Factorial

open Expecto

open Swensen.Unquote

open Mono.Cecil

open FSharpIL
#endif
(**
# Hello World

The following example creates a simple .NET 5 console application.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.PortableExecutable

let example() =
    metadata {
        let! assm =
            setAssembly
                { Name = AssemblyName.ofStr "Factorial"
                  HashAlgId = ()
                  Version = Version(1, 2, 6, 24)
                  Flags = ()
                  PublicKey = None
                  Culture = NullCulture }

        let! mscorlib = SystemAssembly.Net5_0.private_corelib
        let! object = SystemTypes.object mscorlib
        let! dictionary =
            { ResolutionScope = ResolutionScope.AssemblyRef mscorlib
              TypeName = Identifier.ofStr "Dictionary`2"
              TypeNamespace = "System.Collections.Generic" }
            |> referenceType

        let dictionary_u4_u4 = GenericInst.typeRef false dictionary [ EncodedType.U4; EncodedType.U4 ]

        let! factorial =
            { ClassName = Identifier.ofStr "CachedFactorial"
              TypeNamespace = "Factorial"
              Access = TypeVisibility.Public
              Flags = Flags.staticClass ClassFlags.None
              Extends = Extends.TypeRef object }
            |> addStaticClass

        let! _ =
            { FieldName = Identifier.ofStr "cache"
              Flags =
                { Visibility = Public
                  NotSerialized = false
                  SpecialName = false }
                |> Flags.staticField
              Signature = EncodedType.GenericInst dictionary_u4_u4 |> FieldSignature.create }
            |> StaticField
            |> addField factorial

        let calculateBody, setCalculateBody = MethodBody.mutableBody()

        let! helper =
            { MethodName = Identifier.ofStr "CalculateHelper"
              ImplFlags = MethodImplFlags.None
              Flags = { Visibility = Public; HideBySig = true } |> Flags.staticMethod
              ParamList =
                fun _ i ->
                    match i with
                    | 0 -> { Flags = ParamFlags.None; ParamName = "num" }
                    | _ -> { Flags = ParamFlags.None; ParamName = "accumulator" }
                    |> Param
              Signature =
                StaticMethodSignature(
                    Default,
                    ReturnType.itemU4,
                    ImmutableArray.Create(ParamItem.create EncodedType.U4, ParamItem.create EncodedType.U4)
                )
              Body = calculateBody }
            |> StaticMethod
            |> addMethod factorial

        let! _ =
            { MethodName = Identifier.ofStr "Calculate"
              ImplFlags = MethodImplFlags.None
              Flags = { Visibility = Public; HideBySig = true } |> Flags.staticMethod
              ParamList = fun _ _ -> Param { Flags = ParamFlags.None; ParamName = "num" }
              Signature =
                StaticMethodSignature(
                    Default,
                    ReturnType.itemU4,
                    ParamItem.create EncodedType.U4 |> ImmutableArray.Create
                )
              Body =
                fun content ->
                    let writer = MethodBodyWriter content
                    // TODO: Check if the value is in the cache.

                    writer.Ldarg 0us
                    writer.Ldc_i4 2
                    let target = writer.Bgt_un_s() // Check if the argument "num" is less than two.
                    let pos = writer.ByteCount

                    writer.Ldc_i4 2
                    writer.Ret()

                    target.SetTarget(int32 (writer.ByteCount - pos)) // TODO: Make helper functions for making calculation of offsets easier.

                    writer.Ldarg 0us
                    writer.Ldarg 0us
                    writer.Call helper
                    writer.Ret()
                    { MaxStack = 8us; InitLocals = false }
                |> MethodBody.create }
            |> StaticMethod
            |> addMethod factorial

        setCalculateBody <| fun content ->
            let writer = MethodBodyWriter content
            writer.Ldarg 0us
            writer.Ldc_i4 1
            let target = writer.Bgt_un_s() // If "num" <= 1, return the accumulator value.
            let pos = writer.ByteCount

            writer.Ldarg 1us
            writer.Ret()

            target.SetTarget(int32 (writer.ByteCount - pos))

            writer.Ldarg 0us
            writer.Ldc_i4 1
            writer.conv_u4()
            writer.Sub()

            // TODO: Maybe store this in a local variable instead of duplicating it.
            writer.Ldarg 0us
            writer.Ldc_i4 1
            writer.conv_u4()
            writer.Sub()

            writer.Ldarg 1us
            writer.Mul()
            writer.Tail_call helper
            writer.Ret()
            { MaxStack = 8us; InitLocals = false }
    }
    |> CliMetadata.createMetadata
        { Name = Identifier.ofStr "Factorial.dll"
          Mvid = Guid.NewGuid() }
    |> ValidationResult.get
    |> PEFile.ofMetadata ImageFileFlags.dll

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    testList "factorial" [
        testCaseCecil example "has correct field names" <| fun metadata ->
            let expected = [ "cache" ]
            let actual =
                metadata.Types
                |> Seq.collect (fun tdef -> tdef.Fields)
                |> Seq.map (fun field -> field.Name)
                |> List.ofSeq
            test <@ expected = actual @>

        testCaseFile example "can save to disk" __SOURCE_DIRECTORY__ "exout" "Factorial.dll" ignore

        // TODO: Figure out if a property test can be used to test factorial calculation.
        testCaseLoad example "method can be called" <| fun assm ->
            let calculate = assm.GetType("Factorial.CachedFactorial").GetMethod("Calculate")
            let expected = 6u
            let actual = calculate.Invoke(null, [| 3u |]) |> unbox
            test <@ expected = actual @>
    ]
#endif
