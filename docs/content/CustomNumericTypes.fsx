(*** hide ***)
#if !COMPILED
#r "../Dependencies.fsx"
#else
module FSharpIL.CustomNumericTypes

open Expecto

open Swensen.Unquote

open System.IO

open FSharpIL
#endif
(**
# Custom Numeric Types

The following example showcases the generation of structs and implementation of interfaces by creating custom numeric types.
Instead of the usual functions that can check for CLS violations and produce warnings, this example uses functions that skip all
checks and throw exceptions on errors instead.

## Example
*)
open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.Metadata.Unchecked
open FSharpIL.Metadata.UncheckedExn
open FSharpIL.PortableExecutable

let example() =
    let builder =
        { Name = Identifier.ofStr "CustomNumbers.dll"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    // TODO: Make versions of SystemAssembly and SystemType modules that don't use warning builders.
    let throwaway = ImmutableArray.CreateBuilder<_>()

    let mscorlib = SystemAssembly.Net5_0.private_corelib builder throwaway
    let valueType =
        { TypeName = Identifier.ofStr "ValueType"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder
    let icomparable_1 =
        { TypeName = Identifier.ofStr "IComparable`1"
          TypeNamespace = "System"
          ResolutionScope = ResolutionScope.AssemblyRef mscorlib }
        |> referenceType builder

    // TODO: In the future, this would be a good example to showcase functions to generate XML documentation.
    let posInt = // TODO: Use helper function to define a struct instead.
        let info =
            { StructDef.Access = TypeVisibility.Public
              Flags = ClassFlags() |> Flags.valueType
              StructName = Identifier.ofStr "PosInt"
              TypeNamespace = "CustomNumbers" }
        Unsafe.AddStruct(builder, valueType, info)

    let value =
        { Flags = Flags.staticField(FieldFlags Private)
          FieldName = Identifier.ofStr "value"
          Signature = FieldSignature.create EncodedType.U4 }
        |> StaticField
        :> IField<StructDef> // TODO: Figure out how to avoid casting when adding members to structs.
        |> addField builder posInt

    let op_Addition =
        { Body =
            failwith "TODO: Add numbers"
          ImplFlags = MethodImplFlags()
          Flags = Flags.staticMethod(StaticMethodFlags(Public, SpecialName, true))
          MethodName = Identifier.ofStr "op_Addition"
          Signature = StaticMethodSignature(MethodCallingConventions.Default, failwith "return", failwith "parameters")
          ParamList =
            fun _ i ->
                { Flags = ParamFlags()
                  ParamName =
                    match i with
                    | 0 -> "a"
                    | _ -> "b" }
                |> Param }
        |> StaticMethod
        |> addMethod builder posInt

    // setTargetFramework

    CliMetadata builder |> PEFile.ofMetadata ImageFileFlags.dll

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    let example' =
        lazy
            try example()
            with
            | ValidationErrorException err as ex -> raise(Exception(err.ToString(), ex))
    let metadata = lazy PEFile.toCecilModule example'.Value
    let context =
        lazy
            let (ctx, assembly) = PEFile.toLoadContext "factorial" example'.Value
            {| Context = ctx; Assembly = assembly |}

    afterRunTests <| fun() ->
        context.Value.Context.Unload()
        metadata.Value.Dispose()

    testList "custom numeric types" [
    ]
#endif
