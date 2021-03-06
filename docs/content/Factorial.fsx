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
                    writer.Ret()
                    //failwith "TODO: Figure out how to make this method call itself."
                    { MaxStack = 8us; InitLocals = false }
                |> MethodBody.create }
            |> StaticMethod
            |> addMethod factorial

        // TODO: Do more factorial things
        ()
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

        //testCaseLoad example "method can be called" <| fun assm ->
        //    failwith "TODO: Figure out what method to call and how to pass arguments."
    ]
#endif
