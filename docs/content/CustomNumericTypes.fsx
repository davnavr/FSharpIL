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
open System.Xml

open FSharpIL.Metadata
open FSharpIL.Metadata.CliMetadata
open FSharpIL.Metadata.Unchecked
open FSharpIL.Metadata.UncheckedExn
open FSharpIL.PortableExecutable
open FSharpIL.XmlDoc

let example() =
    let builder =
        { Name = Identifier.ofStr "CustomNumbers.dll"
          Mvid = Guid.NewGuid() }
        |> CliMetadataBuilder

    let assemblyName = AssemblyName.ofStr "CustomNumbers"
    let assembly =
        { Name = assemblyName
          HashAlgId = ()
          Version = Version(3, 14, 15, 9265)
          Flags = ()
          PublicKey = None
          Culture = NullCulture }
        |> setAssembly builder

    let struct (mscorlib, _) =
        let token =
            PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
            |> builder.Blobs.MiscBytes.GetOrAdd
            |> PublicKeyOrToken
        AssemblyRef (
            Version(5, 0, 0, 0),
            AssemblyName.ofStr "System.Runtime",
            token
        ) |> referenceAssembly builder
    let valueType =
        TypeRef(ResolutionScope.AssemblyRef mscorlib, Identifier.ofStr "ValueType", "System")
        |> referenceType builder
    let icomparable_1 =
        TypeRef(ResolutionScope.AssemblyRef mscorlib, Identifier.ofStr "IComparable`1", "System")
        |> referenceType builder
    let stringBuilder =
        TypeRef(ResolutionScope.AssemblyRef mscorlib, Identifier.ofStr "StringBuilder", "System.Text")
        |> referenceType builder

    // new()
    let struct (stringBuilder_ctor, _) =
        { Class = MemberRefParent.TypeRef stringBuilder
          MemberName = Identifier.ofStr ".ctor"
          Signature =
            MethodRefDefaultSignature(true, false, ReturnType.itemVoid)
            |> builder.Blobs.MethodRefSig.GetOrAdd }
        |> referenceDefaultMethod builder
    // member _.ToString(): string
    let struct (stringBuilder_ToString, _) =
        { Class = MemberRefParent.TypeRef stringBuilder
          MemberName = Identifier.ofStr "ToString"
          Signature =
            MethodRefDefaultSignature(true, false, ReturnType.encoded EncodedType.String)
            |> builder.Blobs.MethodRefSig.GetOrAdd }
        |> referenceDefaultMethod builder
    let stringBuilder_Append item =
        let signature =
            MethodRefDefaultSignature (
                true,
                false,
                EncodedType.typeRefClass stringBuilder |> ReturnType.encoded,
                ParamItem.create item
            )
        let struct (append, _) =
            { Class = MemberRefParent.TypeRef stringBuilder
              MemberName = Identifier.ofStr "Append"
              Signature = builder.Blobs.MethodRefSig.GetOrAdd signature }
            |> referenceDefaultMethod builder
        append
    // member _.Append(_: int32): System.Text.StringBuilder
    let stringBuilder_Append_I4 = stringBuilder_Append EncodedType.I4
    // member _.Append(_: char): System.Text.StringBuilder
    let stringBuilder_Append_Char = stringBuilder_Append EncodedType.Char

    // TODO: In the future, this would be a good example to showcase functions to generate XML documentation.

    // [<Struct>] type Fraction
    let fraction = // TODO: Use helper function to define a struct instead.
        let info =
            { StructDef.Access = TypeVisibility.Public
              Flags = ClassFlags() |> Flags.valueType
              StructName = Identifier.ofStr "Fraction"
              TypeNamespace = "CustomNumbers" }
        Unsafe.AddStruct(builder, valueType, info)

    let fractionEncoded = fraction.AsTypeIndex() |> EncodedType.typeDefStruct

    let icomparable_fraction =
        GenericInst.typeRef1 false icomparable_1 fractionEncoded
        |> TypeSpec.GenericInst
        |> builder.Blobs.TypeSpec.GetOrAdd
        |> addTypeSpec builder

    let readonly =
        TypeRef (
            ResolutionScope.AssemblyRef mscorlib,
            Identifier.ofStr "IsReadOnlyAttribute",
            "System.Runtime.CompilerServices"
        )
        |> referenceType builder
    // new()
    let struct (readonly_ctor, _) =
        { Class = MemberRefParent.TypeRef readonly
          MemberName = Identifier.ofStr ".ctor"
          Signature =
            MethodRefDefaultSignature(true, false, ReturnType.itemVoid)
            |> builder.Blobs.MethodRefSig.GetOrAdd }
        |> referenceDefaultMethod builder

    // [<type: System.Runtime.CompilerServices.IsReadOnly>]
    { Parent = CustomAttributeParent.TypeDef(fraction.AsTypeIndex())
      Type = CustomAttributeType.MethodRefDefault readonly_ctor
      Value = ValueNone }
    |> addCustomAttribute builder

    let intfield = FieldSignature.create EncodedType.I4 |> builder.Blobs.FieldSig.GetOrAdd
    // val private (*initonly*) numerator: int32
    let numerator =
        { Flags = FieldFlags(Private, initOnly = true) |> Flags.instanceField
          FieldName = Identifier.ofStr "numerator"
          Signature = intfield }
        |> Struct.addInstanceField builder fraction
    // val private (*initonly*) denominator: int32
    let denominator =
        { Flags = FieldFlags(Private, initOnly = true) |> Flags.instanceField
          FieldName = Identifier.ofStr "denominator"
          Signature = intfield }
        |> Struct.addInstanceField builder fraction
    // TODO: Add properties to access values in numbers example.
    let getter_flags = InstanceMethodFlags(Public, SpecialName, VTableLayout.ReuseSlot, true)
    let intreturn = InstanceMethodSignature ReturnType.itemI4 |> builder.Blobs.MethodDefSig.GetOrAdd
    // member this.get_Numerator(): int32
    let get_numerator =
        let body content =
            let wr = MethodBodyWriter content
            // this.numerator
            wr.Ldarg 0us
            wr.Ldfld numerator
            wr.Ret()
            MethodBody.Default
        InstanceMethod (
            MethodBody.create ValueNone body,
            Flags.instanceMethod getter_flags,
            Identifier.ofStr "get_Numerator",
            intreturn
        )
        |> Struct.addInstanceMethod builder fraction
    let get_denominator =
        let body content =
            let wr = MethodBodyWriter content
            // this.denominator
            wr.Ldarg 0us
            wr.Ldfld denominator
            wr.Ret()
            MethodBody.Default
        InstanceMethod (
            MethodBody.create ValueNone body,
            Flags.instanceMethod getter_flags,
            Identifier.ofStr "get_Denominator",
            intreturn
        )
        |> Struct.addInstanceMethod builder fraction
    let intprop = InstancePropertySignature EncodedType.I4 |> builder.Blobs.PropertySig.GetOrAdd
    // member this.Numerator: int32 with get
    // TODO: Use helper functions for adding properties instead.
    Unsafe.AddInstanceProperty (
        builder,
        fraction.AsTypeIndex(),
        { PropertyName = Identifier.ofStr "Numerator"
          Flags = Unsafe.CreateFlags<InstanceMethodTag, _> System.Reflection.PropertyAttributes.None
          Type = intprop },
        getter = ValueSome get_numerator,
        setter = ValueNone
    )
    |> ignore
    // member this.Denominator: int32 with get
    Unsafe.AddInstanceProperty (
        builder,
        fraction.AsTypeIndex(),
        { PropertyName = Identifier.ofStr "Denominator"
          Flags = Unsafe.CreateFlags<InstanceMethodTag, _> System.Reflection.PropertyAttributes.None
          Type = intprop },
        getter = ValueSome get_denominator,
        setter = ValueNone
    )
    |> ignore

    // new(numerator: int32, denominator: int32)
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
            MethodBody.Default
        let signature = ObjectConstructorSignature(ParamItem.create EncodedType.I4, ParamItem.create EncodedType.I4)
        ObjectConstructor (
            body = MethodBody.create ValueNone body,
            implFlags = MethodImplFlags(),
            flags = (ConstructorFlags(Public, true) |> Flags.constructor),
            signature = builder.Blobs.MethodDefSig.GetOrAdd signature,
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

    // static member op_Multiply(a: Fraction, b: Fraction): Fraction
    let multiply_body content =
        let wr = MethodBodyWriter content
        // new Fraction(a.numerator * b.numerator, a.denominator * b.denominator)
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
        MethodBody.Default
    let multiply_sig =
        let parameters =
            ImmutableArray.Create (
                ParamItem.create fractionEncoded,
                ParamItem.create fractionEncoded
            )
        let signature = StaticMethodSignature(MethodCallingConventions.Default, ReturnType.encoded fractionEncoded, parameters)
        builder.Blobs.MethodDefSig.GetOrAdd signature

    StaticMethod (
        MethodBody.create ValueNone multiply_body,
        Flags.staticMethod(StaticMethodFlags(Public, SpecialName, true)),
        Identifier.ofStr "op_Multiply",
        multiply_sig,
        fun _ i ->
            { Flags = ParamFlags()
              ParamName =
                match i with
                | 0 -> "a"
                | _ -> "b" }
            |> Param
    )
    |> Struct.addStaticMethod builder fraction
    |> ignore

    // static member op_Explicit(fraction: Fraction): System.Single
    let conv_single_body content =
        let wr = MethodBodyWriter content
        // (float32 this.numerator) / (float32 this.denominator)
        wr.Ldarg 0us
        wr.Ldfld numerator
        wr.Conv_r4()
        wr.Ldarg 0us
        wr.Ldfld denominator
        wr.Conv_r4()
        wr.Div()
        wr.Ret()
        MethodBody.Default
    StaticMethod (
        MethodBody.create ValueNone conv_single_body,
        Flags.staticMethod(StaticMethodFlags(Public, SpecialName, true)),
        Identifier.ofStr "op_Explicit",
        StaticMethodSignature(ReturnType.encoded EncodedType.R4, ParamItem.create fractionEncoded) |> builder.Blobs.MethodDefSig.GetOrAdd,
        fun _ _ -> Param { Flags = ParamFlags(); ParamName = "fraction" }
    )
    |> Struct.addStaticMethod builder fraction
    |> ignore
    // static member op_Explicit(fraction: Fraction): System.Double
    let conv_double_body content =
        let wr = MethodBodyWriter content
        // (float this.numerator) / (float this.denominator)
        wr.Ldarg 0us
        wr.Ldfld numerator
        wr.Conv_r8()
        wr.Ldarg 0us
        wr.Ldfld denominator
        wr.Conv_r8()
        wr.Div()
        wr.Ret()
        MethodBody.Default
    StaticMethod (
        MethodBody.create ValueNone conv_double_body,
        Flags.staticMethod(StaticMethodFlags(Public, SpecialName, true)),
        Identifier.ofStr "op_Explicit",
        StaticMethodSignature(ReturnType.encoded EncodedType.R8, ParamItem.create fractionEncoded) |> builder.Blobs.MethodDefSig.GetOrAdd,
        fun _ _ -> Param { Flags = ParamFlags(); ParamName = "fraction" }
    )
    |> Struct.addStaticMethod builder fraction
    |> ignore

    // interface System.IComparable<Fraction>
    Struct.implementSpec builder fraction icomparable_fraction |> ignore

    // member (*virtual*) this.CompareTo(other: Fraction): Fraction
    let compare_body content =
        let wr = MethodBodyWriter content
        wr.Ldarg 0us
        wr.Ldfld denominator // this.denominator
        wr.Ldarg 1us
        wr.Ldfld denominator // other.denominator
        let ne = wr.Bne_un_s()
        let ne_pos = wr.ByteCount

        // Both denominators are equal here
        // this.numerator - other.numerator
        wr.Ldarg 0us
        wr.Ldfld numerator
        wr.Ldarg 1us
        wr.Ldfld numerator
        wr.Sub()
        wr.Ret()

        ne.SetTarget(int32 (wr.ByteCount - ne_pos))

        // Both denominators are not equal here
        // TODO: Account for fact that denominators might be negative when comparing fractions.
        // (this.numerator * other.denominator) - (other.numerator * this.denominator)
        wr.Ldarg 0us
        wr.Ldfld numerator
        wr.Ldarg 1us
        wr.Ldfld denominator
        wr.Mul()
        wr.Ldarg 1us
        wr.Ldfld numerator
        wr.Ldarg 0us
        wr.Ldfld denominator
        wr.Mul()
        wr.Sub()
        wr.Ret()
        MethodBody.Default
    InstanceMethod (
        MethodBody.create ValueNone compare_body,
        InstanceMethodFlags(Public, NoSpecialName, ReuseSlot, true, true) |> Flags.instanceMethod,
        Identifier.ofStr "CompareTo",
        InstanceMethodSignature(ReturnType.itemI4, ParamItem.create fractionEncoded) |> builder.Blobs.MethodDefSig.GetOrAdd,
        fun _ _ -> Param { Flags = ParamFlags(); ParamName = "other" }
    )
    |> Struct.addInstanceMethod builder fraction
    |> ignore

    // override this.ToString(): string
    let tostring_body content =
        let wr = MethodBodyWriter content
        // new System.Text.StringBuilder().Append(this.numerator).Append('/').Append(this.denominator).ToString()
        wr.Newobj stringBuilder_ctor
        wr.Ldarg 0us
        wr.Ldfld numerator
        wr.Call stringBuilder_Append_I4
        wr.Ldc_i4 '/'
        wr.Call stringBuilder_Append_Char
        wr.Ldarg 0us
        wr.Ldfld denominator
        wr.Call stringBuilder_Append_I4
        wr.Callvirt stringBuilder_ToString
        wr.Ret()
        MethodBody.Default
    InstanceMethod (
        MethodBody.create ValueNone tostring_body,
        InstanceMethodFlags(
            Public,
            NoSpecialName,
            ReuseSlot,
            hideBySig = true,
            isVirtual = true
        ) |> Flags.instanceMethod,
        Identifier.ofStr "ToString",
        InstanceMethodSignature(ReturnType.encoded EncodedType.String) |> builder.Blobs.MethodDefSig.GetOrAdd
    )
    |> Struct.addInstanceMethod builder fraction
    |> ignore

    // TODO: Figure out how to avoid duplicating code for setting target framework.
    let tfm =
        TypeRef (
            ResolutionScope.AssemblyRef mscorlib,
            Identifier.ofStr "TargetFrameworkAttribute",
            "System.Runtime.Versioning"
        )
        |> referenceType builder
    // new(_: string)
    let struct(tfm_ctor, _) =
        { Class = MemberRefParent.TypeRef tfm
          MemberName = Identifier.ofStr ".ctor"
          Signature =
            let parameters = ImmutableArray.Create(ParamItem.create EncodedType.String)
            let signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid, parameters)
            builder.Blobs.MethodRefSig.GetOrAdd signature }
        |> referenceDefaultMethod builder

    setTargetFramework builder assembly tfm_ctor ".NETCoreApp,Version=v5.0"
    
    let metadata = CliMetadata builder

    use doc = new MemoryStream()

    Doc.generateDoc
        (fun i xml ->
            match i with
            | DocMember.Type t when t.Value = fraction.Value ->
                xml.WriteElementString("summary", "Represents a simple fraction.")
                xml.WriteStartElement "remarks"
                xml.WriteRaw "The "
                Doc.see i metadata xml
                xml.WriteRaw "type supports conversions to 4-byte and 8-byte floating point numbers."
                xml.WriteEndElement()
            | _ -> ())
        metadata
        (XmlWriter.Create doc)
        assemblyName
        [
            DocMember.Type(fraction.AsTypeIndex())
        ]

    // TODO: Return XML documentation as well.
    PEFile.ofMetadata ImageFileFlags.dll metadata

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    let example' = lazy example()
    let metadata = lazy PEFile.toCecilModule example'.Value
    let fraction = lazy metadata.Value.GetType("CustomNumbers", "Fraction")

    afterRunTests <| fun() -> if metadata.IsValueCreated then metadata.Value.Dispose()

    testList "custom numeric types" [
        testCase "can save to disk" <| fun() ->
            let path = Path.Combine(__SOURCE_DIRECTORY__, "exout", "CustomNumbers.dll")
            WritePE.toPath path example'.Value

        testCase "assembly name is correct" <| fun() -> test <@ metadata.Value.Assembly.Name.Name = "CustomNumbers" @>

        testCase "fraction methods are in correct order" <| fun() ->
            let expected =
                [
                    "get_Numerator"
                    "get_Denominator"
                    ".ctor"
                    "op_Multiply"
                    "op_Explicit"
                    "op_Explicit"
                    "CompareTo"
                    "ToString"
                ]
            let actual =
                fraction.Value.Methods
                |> Seq.map (fun method -> method.Name)
                |> List.ofSeq
            expected =! actual

        testCase "property names are correct" <| fun() ->
            let expected = [ "Numerator"; "Denominator" ]
            let actual =
                fraction.Value.Properties
                |> Seq.map (fun prop -> prop.Name)
                |> List.ofSeq
            expected =! actual
    ]
#endif
