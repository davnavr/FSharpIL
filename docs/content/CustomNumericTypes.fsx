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
    let valueType = TypeRef.createReflectedRow builder mscorlib' typeof<ValueType>
    let icomparable_1 = TypeRef.createReflectedRow builder mscorlib' typedefof<IComparable<_>>
    let stringBuilder = TypeRef.createReflectedRow builder mscorlib' typeof<System.Text.StringBuilder>

    // new()
    let struct (stringBuilder_ctor, _) =
        { Class = MemberRefParent.TypeRef stringBuilder
          MemberName = Identifier.ofStr ".ctor"
          Signature =
            MethodRefDefaultSignature(true, false, ReturnType.itemVoid)
            |> builder.Blobs.MethodRefSig.GetOrAdd }
        |> MethodRef.addRowDefault builder
    // member _.CompareTo(_: 'T): int32
    let struct (icomparable_compare, _) =
        let signature =
            MethodRefGenericSignature (
                true,
                false,
                1u,
                ReturnType.encoded EncodedType.I4,
                ImmutableArray.Create(ParamItem.var 0u)
            )
        { Class = MemberRefParent.TypeRef icomparable_1
          MemberName = Identifier.ofStr "CompareTo"
          Signature = builder.Blobs.MethodRefSig.GetOrAdd signature }
        |> MethodRef.addRowGeneric builder
    // member _.ToString(): string
    let struct (stringBuilder_ToString, _) =
        { Class = MemberRefParent.TypeRef stringBuilder
          MemberName = Identifier.ofStr "ToString"
          Signature =
            MethodRefDefaultSignature(true, false, ReturnType.encoded EncodedType.String)
            |> builder.Blobs.MethodRefSig.GetOrAdd }
        |> MethodRef.addRowDefault builder
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
            |> MethodRef.addRowDefault builder
        append
    // member _.Append(_: int32): System.Text.StringBuilder
    let stringBuilder_Append_I4 = stringBuilder_Append EncodedType.I4
    // member _.Append(_: char): System.Text.StringBuilder
    let stringBuilder_Append_Char = stringBuilder_Append EncodedType.Char

    // TODO: In the future, this would be a good example to showcase functions to generate XML documentation.

    // [<Struct>] type Fraction
    let fraction = // TODO: Use safe helper function to define a struct instead.
        { StructDef.Access = TypeVisibility.Public
          Flags = ClassFlags() |> Flags.valueType
          StructName = Identifier.ofStr "Fraction"
          TypeNamespace = "CustomNumbers" }
        |> Unsafe.addStructRow builder (Extends.TypeRef valueType)
    let fraction' = Struct.typeIndex fraction

    let fractionEncoded = EncodedType.typeDefStruct fraction'

    let icomparable t =
        GenericInst.typeRef1 false icomparable_1 t
        |> TypeSpec.GenericInst
        |> TypeSpec.createRow builder
        |> InterfaceIndex.TypeSpec

    let readonly = TypeRef.createReflectedRow builder mscorlib' typeof<System.Runtime.CompilerServices.IsReadOnlyAttribute>
    // new()
    let struct (readonly_ctor, _) =
        { Class = MemberRefParent.TypeRef readonly
          MemberName = Identifier.ofStr ".ctor"
          Signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid) |> builder.Blobs.MethodRefSig.GetOrAdd }
        |> MethodRef.addRowDefault builder

    // [<type: System.Runtime.CompilerServices.IsReadOnly>]
    { Parent = CustomAttributeParent.TypeDef fraction'
      Type = CustomAttributeType.MethodRefDefault readonly_ctor
      Value = ValueNone }
    |> CustomAttribute.addRow builder

    let intfield = FieldSignature.create EncodedType.I4 |> builder.Blobs.FieldSig.GetOrAdd
    // val private (*initonly*) numerator: int32
    let numerator =
        { Flags = FieldFlags(Private, initOnly = true)
          FieldName = Identifier.ofStr "numerator"
          Signature = intfield }
        |> InstanceField.addRow builder (InstanceMemberOwner.Struct fraction)
    // val private (*initonly*) denominator: int32
    let denominator =
        { Flags = FieldFlags(Private, initOnly = true)
          FieldName = Identifier.ofStr "denominator"
          Signature = intfield }
        |> InstanceField.addRow builder (InstanceMemberOwner.Struct fraction)
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
        |> InstanceMethod.addRow builder (InstanceMemberOwner.Struct fraction)
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
        |> InstanceMethod.addRow builder (InstanceMemberOwner.Struct fraction)
    let intprop = InstancePropertySignature EncodedType.I4 |> builder.Blobs.PropertySig.GetOrAdd
    // member this.Numerator: int32 with get
    // TODO: Use safe helper functions for adding properties instead.
    Property.createInstanceRow
        builder
        (InstanceMemberOwner.Struct fraction)
        (InstanceMethodIndex.Instance get_numerator |> ValueSome)
        ValueNone
        ImmutableArray.Empty
        { PropertyName = Identifier.ofStr "Numerator"
          Flags = Unsafe.createFlags<InstanceMethodTag, _> System.Reflection.PropertyAttributes.None
          Type = intprop }
    |> ignore
    // member this.Denominator: int32 with get
    Property.createInstanceRow
        builder
        (InstanceMemberOwner.Struct fraction)
        (InstanceMethodIndex.Instance get_denominator |> ValueSome)
        ValueNone
        ImmutableArray.Empty
        { PropertyName = Identifier.ofStr "Denominator"
          Flags = Unsafe.createFlags<InstanceMethodTag, _> System.Reflection.PropertyAttributes.None
          Type = intprop }
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
        |> ObjectConstructor.addRow builder (InstanceMemberOwner.Struct fraction)

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
    |> StaticMethod.addRow builder (StaticMemberOwner.Struct fraction)
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
    let conv_single_signature =
        StaticMethodSignature (
            ReturnType.encoded EncodedType.R4,
            ParamItem.create fractionEncoded
        )
        |> builder.Blobs.MethodDefSig.GetOrAdd
    StaticMethod (
        MethodBody.create ValueNone conv_single_body,
        Flags.staticMethod(StaticMethodFlags(Public, SpecialName, true)),
        Identifier.ofStr "op_Explicit",
        conv_single_signature,
        fun _ _ -> Param { Flags = ParamFlags(); ParamName = "fraction" }
    )
    |> StaticMethod.addRow builder (StaticMemberOwner.Struct fraction)
    |> ignore
    // static member op_Explicit(fraction: Fraction): System.Double
    let conv_double =
        let body content =
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
        let signature =
            StaticMethodSignature (
                ReturnType.encoded EncodedType.R8,
                ParamItem.create fractionEncoded
            )
            |> builder.Blobs.MethodDefSig.GetOrAdd
        StaticMethod (
            MethodBody.create ValueNone body,
            Flags.staticMethod(StaticMethodFlags(Public, SpecialName, true)),
            Identifier.ofStr "op_Explicit",
            signature,
            ParamList.named [| "fraction" |]
        )
        |> StaticMethod.addRow builder (StaticMemberOwner.Struct fraction)

    // interface System.IComparable<Fraction>
    InterfaceImpl.addRow
        builder
        (InterfaceImplementor.Struct fraction)
        (icomparable fractionEncoded)
    |> ignore

    // member (*virtual*) this.CompareTo(other: Fraction): System.Int32
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
    |> InstanceMethod.addRow builder (InstanceMemberOwner.Struct fraction)
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
    |> InstanceMethod.addRow builder (InstanceMemberOwner.Struct fraction)
    |> ignore

    // interface System.IComparable<System.Double> with
    InterfaceImpl.addRow
        builder
        (InterfaceImplementor.Struct fraction)
        (icomparable EncodedType.R8)
    |> ignore

    // member private (*final*) (*virtual*) (*hidebysig*) (*newslot*) this.``System.IComparable<System.Double>.CompareTo``
    //     (
    //         other: System.Double
    //     ): System.Int32
    let compare_float =
        let body content =
            let wr = MethodBodyWriter content
            (* Since `this` is a pointer, we need to get the actual value with `ldobj` *)
            // let value: System.Double = Fraction.op_Explicit this
            wr.Ldarg 0us
            wr.Ldobj(Struct.typeIndex fraction)
            wr.Call conv_double

            // value > other
            wr.Dup()
            wr.Ldarg 1us
            let gt = wr.Bgt_s()

            // value < other
            wr.Ldarg 1us
            let lt = wr.Blt_s()

            // 0
            wr.Ldc_i4 0
            wr.Ret()

            // -1
            let lt' = Label wr
            lt'.SetTarget lt
            wr.Ldc_i4 -1
            wr.Ret()

            // 1
            let gt' = Label wr
            gt'.SetTarget gt
            wr.Ldc_i4 1
            wr.Ret()
            MethodBody 3us
        let signature = InstanceMethodSignature(ReturnType.encoded EncodedType.I4, ParamItem.create EncodedType.R8)
        FinalMethod (
            MethodBody.create ValueNone body,
            Flags.finalMethod(InstanceMethodFlags(Private, NoSpecialName, NewSlot, hideBySig = true, isVirtual = true)),
            Identifier.ofStr "System.IComparable<System.Double>.CompareTo",
            builder.Blobs.MethodDefSig.GetOrAdd signature,
            ParamList.named [| "other" |]
        )
        |> FinalMethod.addRow builder (InstanceMemberOwner.Struct fraction)

    MethodImpl.add
        builder
        (Struct.typeIndex fraction)
        (MethodDefOrRef.Def(FinalMethod.methodIndex compare_float))
        (MethodDefOrRef.RefGeneric icomparable_compare)

    let tfm = TypeRef.createReflectedRow builder mscorlib' typeof<System.Runtime.Versioning.TargetFrameworkAttribute>
    // new(_: string)
    let struct(tfm_ctor, _) =
        { Class = MemberRefParent.TypeRef tfm
          MemberName = Identifier.ofStr ".ctor"
          Signature =
            let parameters = ImmutableArray.Create(ParamItem.create EncodedType.String)
            let signature = MethodRefDefaultSignature(true, false, ReturnType.itemVoid, parameters)
            builder.Blobs.MethodRefSig.GetOrAdd signature }
        |> MethodRef.addRowDefault builder

    Assembly.setTargetFramework builder assembly tfm_ctor ".NETCoreApp,Version=v5.0"
    
    let metadata = CliMetadata builder

    use doc = new MemoryStream()
    use xwriter = XmlWriter.Create doc

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
        xwriter
        assemblyName
        [
            DocMember.Type fraction'
        ]

    // TODO: Return XML documentation as well.
    {| File = PEFile.ofMetadata ImageFileFlags.dll metadata
       Xml = doc.GetBuffer() |}

(*** hide ***)
#if COMPILED
[<Tests>]
let tests =
    let example' = lazy example()
    let metadata = lazy PEFile.toCecilModule example'.Value.File
    let fraction = lazy metadata.Value.GetType("CustomNumbers", "Fraction")

    afterRunTests <| fun() -> if metadata.IsValueCreated then metadata.Value.Dispose()

    testList "custom numeric types" [
        testCase "can save to disk" <| fun() ->
            let dir = Path.Combine(__SOURCE_DIRECTORY__, "exout")
            WritePE.toPath (Path.Combine(dir, "CustomNumbers.dll")) example'.Value.File
            File.WriteAllBytes(Path.Combine(dir, "CustomNumbers.xml"), example'.Value.Xml)

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
                    "System.IComparable<System.Double>.CompareTo"
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
