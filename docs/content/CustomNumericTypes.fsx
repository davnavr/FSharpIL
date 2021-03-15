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
open FSharpIL.Metadata.UncheckedExn
open FSharpIL.PortableExecutable

let example() =
    let builder =
        { Name = Identifier.ofStr "CustomNumbers.dll"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    let assembly =
        { Name = AssemblyName.ofStr "CustomNumbers"
          HashAlgId = ()
          Version = Version(3, 14, 15, 9265)
          Flags = ()
          PublicKey = None
          Culture = NullCulture }
        |> setAssembly builder

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
    let fraction = // TODO: Use helper function to define a struct instead.
        let info =
            { StructDef.Access = TypeVisibility.Public
              Flags = ClassFlags() |> Flags.valueType
              StructName = Identifier.ofStr "PosInt"
              TypeNamespace = "CustomNumbers" }
        Unsafe.AddStruct(builder, valueType, info)
    let fractionEncoded = EncodedType.typeDefStruct fraction

    let numerator =
        { Flags = Flags.instanceField(FieldFlags Private)
          FieldName = Identifier.ofStr "numerator"
          Signature = FieldSignature.create EncodedType.I4 }
        |> Struct.addInstanceField builder fraction
    let denominator =
        { Flags = Flags.instanceField(FieldFlags Private)
          FieldName = Identifier.ofStr "denominator"
          Signature = FieldSignature.create EncodedType.I4 }
        |> Struct.addInstanceField builder fraction

    // TODO: Add properties to access values in numbers example.

    let ctor =
        let body content =
            let wr = MethodBodyWriter content
            wr.Ldarg 0us
            wr.Ldarg 1us
            wr.Stfld numerator
            wr.Ldarg 0us
            wr.Ldarg 2us
            wr.Stfld denominator
            wr.Ret()
            { MaxStack = 1us; InitLocals = false }
        Constructor (
            body = MethodBody.create body,
            implFlags = MethodImplFlags(),
            flags = (ConstructorFlags(Public, true) |> Flags.constructor),
            signature = ObjectConstructorSignature(ParamItem.create EncodedType.I4, ParamItem.create EncodedType.I4),
            paramList =
                fun _ i ->
                    { Flags = ParamFlags()
                      ParamName =
                        match i with
                        | 0 -> "numerator"
                        | _ -> "denominator" }
                    |> Param
        )
        |> Struct.addConstructor builder fraction

    { Body =
        fun content ->
            let wr = MethodBodyWriter content
            wr.Ldarg 0us
            wr.Ldfld numerator
            wr.Ldarg 1us
            wr.Ldfld numerator
            wr.Mul()
            wr.Ldarg 0us
            wr.Ldfld denominator
            wr.Ldarg 1us
            wr.Ldfld denominator
            wr.Mul()
            wr.Newobj ctor
            wr.Ret()
            { MaxStack = 2us; InitLocals = false }
        |> MethodBody.create
      ImplFlags = MethodImplFlags()
      Flags = Flags.staticMethod(StaticMethodFlags(Public, SpecialName, true))
      MethodName = Identifier.ofStr "op_Multiply"
      Signature =
        let parameters =
            ImmutableArray.Create (
                ParamItem.create fractionEncoded,
                ParamItem.create fractionEncoded
            )
        StaticMethodSignature(MethodCallingConventions.Default, ReturnType.encoded fractionEncoded, parameters)
      ParamList =
        fun _ i ->
            { Flags = ParamFlags()
              ParamName =
                match i with
                | 0 -> "a"
                | _ -> "b" }
            |> Param }
    |> Struct.addStaticMethod builder fraction
    |> ignore

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
        if context.IsValueCreated then context.Value.Context.Unload()
        metadata.Value.Dispose()

    testList "custom numeric types" [
        testCase "can save to disk" <| fun() ->
            let path = Path.Combine(__SOURCE_DIRECTORY__, "exout", "CustomNumbers.dll")
            WritePE.toPath path example'.Value
    ]
#endif
