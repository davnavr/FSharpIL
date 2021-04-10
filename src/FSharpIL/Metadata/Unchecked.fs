/// <summary>
/// Contains functions for modifying the CLI metadata without CLS checks and warnings.
/// </summary>
[<RequireQualifiedAccess>]
module FSharpIL.Metadata.Unchecked

open System
open System.Collections.Immutable
open System.Reflection

open FSharpIL

[<RequireQualifiedAccess>]
module private DelegateHelpers =
    let implFlags = MethodImplAttributes.Managed ||| MethodImplAttributes.Runtime

    let ctorSignature (builder: CliMetadataBuilder) =
        let parameters =
            [|
                EncodedType.Object
                EncodedType.I // native int
            |]
            |> Array.map ParamItem.create
            |> ImmutableArray.CreateRange
        let signature = MethodDefSignature(true, false, MethodCallingConventions.Default, ReturnType.itemVoid, parameters)
        builder.Blobs.MethodDefSig.GetOrAdd signature

    let ctorParameters (_: ParamItem) (i: int32) =
        { ParamName = if i = 0 then "object" else "method"
          Flags = ParamFlags() }
        |> Param

    let invokeSignature (builder: CliMetadataBuilder) (delegateDef: inref<DelegateDef>) =
        let signature = MethodDefSignature(true, false, Default, delegateDef.ReturnType, delegateDef.Parameters)
        builder.Blobs.MethodDefSig.GetOrAdd signature

// TODO: Figure out if using inref with static methods in Unsafe is a performance improvement.
/// Contains static methods for modifying the CLI metadata without regard for generation of correct metadata.
[<Obsolete>]
[<AbstractClass; Sealed>]
type Unsafe private () = class
    static member val internal ClassConstructorSignature =
        MethodDefSignature(
            false,
            false,
            MethodCallingConventions.Default,
            ReturnTypeItem ReturnTypeVoid.Item,
            ImmutableArray.Empty
        )

    static member internal AddField<'Tag>(builder: CliMetadataBuilder, owner, field) =
        match builder.Field.TryAdd(owner, field) with
        | ValueSome index -> RawIndex<'Tag> index.Value |> Ok
        | ValueNone -> DuplicateFieldError field :> ValidationError |> Error

    static member internal AddField<'Flags>(builder, owner, field: Field<'Flags>) =
        Unsafe.AddField<Field<'Flags>>(builder, owner, field.Row())

    static member internal AddMethod<'Tag>(builder: CliMetadataBuilder, owner, method) =
        match builder.Method.TryAdd(owner, method) with
        | ValueSome index -> RawIndex<'Tag> index.Value |> Ok
        | ValueNone -> DuplicateMethodError method :> ValidationError |> Error

    static member AddMethod<'Body, 'Flags, 'Signature when 'Body :> IMethodBody>
        (
            builder: CliMetadataBuilder,
            owner,
            method: Method<'Body, 'Flags, 'Signature>
        ) =
        Unsafe.AddMethod<Method<'Body, 'Flags, 'Signature>>(builder, owner, method.Definition())

    static member private AddConstructor<'Tag, 'Flags, 'Signature>
        (
            builder: CliMetadataBuilder,
            owner,
            method: Constructor<'Flags, 'Signature>,
            name,
            signature
        ) =
        let row =
            MethodDefRow (
                method.Body,
                method.ImplFlags.Value,
                method.Flags.Value,
                Identifier.ofStr name,
                signature,
                method.ParamList
            )
        Unsafe.AddMethod<'Tag>(builder, owner, row)

    static member AddConstructor(builder: CliMetadataBuilder, owner, method: ObjectConstructor) =
        Unsafe.AddConstructor<ObjectConstructor, ObjectConstructorTag, _> (
            builder,
            owner,
            method,
            ".ctor",
            method.Signature.ChangeTag()
        )

    static member AddConstructor(builder: CliMetadataBuilder, owner, method: ClassConstructor) =
        let signature = builder.Blobs.MethodDefSig.GetOrAdd Unsafe.ClassConstructorSignature
        Unsafe.AddConstructor<ClassConstructor, ClassConstructorTag, _> (
            builder,
            owner,
            method,
            ".cctor",
            signature
        )

    static member AddTypeDef<'Tag>
        (
            builder: CliMetadataBuilder,
            flags,
            typeName,
            typeNamespace,
            extends,
            parent
        ): Result<RawIndex<'Tag>, _> =
        let row =
            TypeDefRow (
                flags,
                typeName,
                typeNamespace,
                extends,
                parent
            )
        match builder.TypeDef.TryAdd row with
        | ValueSome index -> RawIndex<'Tag> index.Value |> Ok
        | ValueNone -> DuplicateTypeDefError row :> ValidationError |> Error

    static member AddTypeDef<'Tag>(builder, flags, typeName, extends) =
        Unsafe.AddTypeDef<'Tag>(builder, flags, typeName, String.Empty, extends, ValueNone)

    // TODO: Add methods for adding struct, delegate, and enum that accept RawIndex<TypeDefRow> if system types are in the current assembly.

    /// <param name="builder"/>
    /// <param name="del">
    /// A <c>TypeRef</c> corresponding to the <see cref="T:System.Delegate"/> or <see cref="T:System.MulticastDelegate"/> type.
    /// </param>
    /// <param name="asyncResult">A <c>Type</c> corresponding to the <see cref="T:System.IAsyncResult"/> type.</param>
    /// <param name="asyncCallback">A <c>Type</c> corresponding to the <see cref="T:System.AsyncCallback"/> type.</param>
    /// <param name="delegateDef"/>
    static member private AddDelegate
        (
            builder,
            del,
            asyncResult,
            asyncCallback,
            delegateDef: inref<DelegateDef>
        ): Result<DelegateInfo, _> =
        let result =
            Unsafe.AddTypeDef<DelegateDef> (
                builder,
                delegateDef.Flags,
                delegateDef.DelegateName,
                delegateDef.TypeNamespace,
                del,
                TypeVisibility.enclosingClass delegateDef.Access
            )
        match result with
        | Ok del' ->
            let trow = del'.AsTypeIndex()

            let ctor = // TODO: Create easier way to make methods and constructors whose implementations are 'runtime'
                let row =
                    MethodDefRow (
                        MethodBody.none,
                        // TODO: Does this correspond to 'runtime' keyword?
                        DelegateHelpers.implFlags,
                        ConstructorFlags(Public, true).Value ||| MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName,
                        Identifier.ofStr ".ctor",
                        DelegateHelpers.ctorSignature builder,
                        DelegateHelpers.ctorParameters
                    )
                builder.Method.TryAdd(trow, row).Value.ChangeTag<ObjectConstructor>()

            let invoke =
                let row =
                    MethodDefRow (
                        MethodBody.none,
                        DelegateHelpers.implFlags,
                        delegateDef.MethodFlags,
                        Identifier.ofStr "Invoke",
                        DelegateHelpers.invokeSignature builder &delegateDef,
                        ParamList.noname
                    )
                builder.Method.TryAdd(trow, row).Value.ChangeTag<InstanceMethod>()

            let beginInvoke =
                let pcount = delegateDef.Parameters.Length

                let parameters =
                    let builder = ImmutableArray.CreateBuilder(pcount + 2)
                    builder.AddRange delegateDef.Parameters
                    builder.Add(ParamItem.create asyncResult) // callback
                    builder.Add(ParamItem.create EncodedType.Object) // objects
                    builder.ToImmutable()

                let signature = MethodDefSignature(true, false, Default, ReturnType.encoded asyncCallback, parameters)

                let row =
                    MethodDefRow (
                        MethodBody.none,
                        DelegateHelpers.implFlags,
                        delegateDef.MethodFlags,
                        Identifier.ofStr "BeginInvoke",
                        builder.Blobs.MethodDefSig.TryAdd signature |> Result.any,
                        fun _ i ->
                            let name =
                                if i = pcount
                                then "callback"
                                elif i = pcount + 1
                                then "objects"
                                else ""
                            Param { ParamName = name; Flags = ParamFlags() }
                    )
                builder.Method.TryAdd(trow, row).Value.ChangeTag<InstanceMethod>()

            let endInvoke =
                let signature =
                    MethodDefSignature (
                        true,
                        false,
                        Default,
                        delegateDef.ReturnType,
                        ImmutableArray.Create(ParamItem.create asyncResult)
                    )
                let row =
                    MethodDefRow (
                        MethodBody.none,
                        DelegateHelpers.implFlags,
                        delegateDef.MethodFlags,
                        Identifier.ofStr "EndInvoke",
                        builder.Blobs.MethodDefSig.TryAdd signature |> Result.any,
                        fun _ _ -> Param { ParamName = "result"; Flags = ParamFlags() }
                    )
                builder.Method.TryAdd(trow, row).Value.ChangeTag<InstanceMethod>()

            DelegateInfo(del', ctor, invoke, beginInvoke, endInvoke) |> Ok
        | Error err -> Error err

    static member AddDelegate(builder, del, asyncResult, asyncCallback, delegateDef) =
        Unsafe.AddDelegate(builder, Extends.TypeRef del, asyncResult, asyncCallback, &delegateDef)

    static member AddDelegate(builder, lookup: TypeLookupCache, delegateDef) =
        // TODO: Cleanup these local variables
        let system = "System"
        let delName = Identifier.ofStr "MulticastDelegate"
        let del = lookup.TryFindType(system, delName)
        let asyncResultName = Identifier.ofStr "IAsyncResult"
        let asyncResult = lookup.TryFindTypeEncoded(system, asyncResultName, false)
        let asyncCallbackName = Identifier.ofStr "AsyncCallback"
        let asyncCallback = lookup.TryFindTypeEncoded(system, asyncCallbackName, false)
        match del, asyncResult, asyncCallback with
        | ValueSome del', ValueSome asyncResult', ValueSome asyncCallback' ->
            let del'' =
                match del' with
                // NOTE: This currently assumes System.Delegate is an abstract class, maybe make a special unsafe Extends case for TypeDefRow?
                | TypeLookupResult.TypeDef tdef -> tdef.ChangeTag() |> Extends.AbstractClass
                | TypeLookupResult.TypeRef tref -> Extends.TypeRef tref
            Unsafe.AddDelegate(builder, del'', asyncResult', asyncCallback', &delegateDef)
        | ValueNone, _, _ -> MissingTypeError(system, delName).ToResult()
        | _, ValueNone, _ -> MissingTypeError(system, asyncResultName).ToResult()
        | _, _, ValueNone -> MissingTypeError(system, asyncCallbackName).ToResult()

    static member private AddEnum(builder, enum, enumDef: inref<EnumDef>): Result<EnumInfo, _> =
        let result =
            Unsafe.AddTypeDef<EnumDef> (
                builder,
                enumDef.Flags,
                enumDef.EnumName,
                enumDef.TypeNamespace,
                enum,
                TypeVisibility.enclosingClass enumDef.Access
            )
        match result with
        | Ok enum' ->
            let trow = enum'.ChangeTag<TypeDefRow>()
            let values = ImmutableArray.CreateBuilder enumDef.Values.Count

            let ivalue =
                let vtype =
                    match enumDef.Values.UnderlyingType with
                    | IntegerType.Bool -> EncodedType.Boolean
                    | IntegerType.Char -> EncodedType.Char
                    | IntegerType.I1 -> EncodedType.I1
                    | IntegerType.U1 -> EncodedType.U1
                    | IntegerType.I2 -> EncodedType.I2
                    | IntegerType.U2 -> EncodedType.U2
                    | IntegerType.I4 -> EncodedType.I4
                    | IntegerType.U4 -> EncodedType.U4
                    | IntegerType.I8 -> EncodedType.I8
                    | IntegerType.U8 -> EncodedType.U8
                    | _ -> sprintf "Invalid underlying type for enumeration \"%O\"" enumDef.EnumName |> invalidArg "enumDef"
                let signature =
                    FieldSignature(ImmutableArray.Empty, vtype)
                    |> builder.Blobs.FieldSig.TryAdd
                    |> Result.any
                let row =
                    FieldRow (
                        FieldAttributes.RTSpecialName ||| FieldAttributes.SpecialName ||| FieldAttributes.Public,
                        Identifier.ofStr "value__",
                        signature
                    )
                builder.Field.TryAdd(trow, row).Value.ChangeTag<InstanceField>()

            for value in enumDef.Values do
                let vtype = TypeDefOrRefOrSpecEncoded.TypeDef trow |> EncodedType.ValueType
                let signature =
                    FieldSignature(ImmutableArray.Empty, vtype)
                    |> builder.Blobs.FieldSig.TryAdd
                    |> Result.any
                let field =
                    FieldRow (
                        FieldAttributes.HasDefault ||| FieldAttributes.Static ||| FieldAttributes.Literal,
                        value.Name,
                        signature
                    )
                let field' = builder.Field.TryAdd(trow, field).Value
                let cvalue = builder.Constant.TryAdd(field', ConstantBlob.Integer value.Value).Value
                values.Add(EnumValueRow(field'.ChangeTag<StaticField>(), cvalue))

            // TODO: Add enum values to constant table.
            EnumInfo(enum', ivalue, values.ToImmutable()) |> Ok
        | Error err -> Error err

    static member AddEnum(builder, enum: RawIndex<TypeRef>, enumDef) = Unsafe.AddEnum(builder, Extends.TypeRef enum, &enumDef)

    //static member AddEnum(builder, lookup: TypeLookupCache, enumDef)

    static member private AddStruct(builder, valueType, structDef: StructDef) =
        Unsafe.AddTypeDef<StructDef>(
            builder,
            structDef.Flags.Value ||| structDef.Access.Tag,
            structDef.StructName,
            structDef.TypeNamespace,
            valueType,
            TypeVisibility.enclosingClass structDef.Access
        )

    static member AddStruct(builder, valueType, structDef) = Unsafe.AddStruct(builder, Extends.TypeRef valueType, structDef)

    static member AddStruct(builder, valueType: RawIndex<TypeDefRow>, structDef) =
        Unsafe.AddStruct(builder, (invalidOp "TODO: Add Extends.TypeDef to allow addition of structs that derive from System.ValueType in the same assembly": Extends), structDef)

    static member ChangeFlagTag<'Flags, 'From, 'To when 'Flags :> Enum>(flags: ValidFlags<'From, 'Flags>) =
        ValidFlags<'To, 'Flags> flags.Value

    static member ChangeIndexTag<'From, 'To>(index: RawIndex<'From>) = index.ChangeTag<'To>()

    static member CreateFlags<'Tag, 'Flags when 'Flags :> Enum> flags = ValidFlags<'Tag, 'Flags> flags

    static member ImplementInterface(builder: CliMetadataBuilder, typeDef: RawIndex<TypeDefRow>, intf) =
        builder.InterfaceImpl.Add(typeDef, intf)

    static member AddProperty
        (
            builder: CliMetadataBuilder,
            parent: RawIndex<TypeDefRow>,
            property: Property<_, _>,
            methods: PropertyMethods
        ) =
        let row = property.Definition()
        match builder.PropertyMap.TryAdd(parent, row) with
        | ValueSome index ->
            if builder.MethodSemantics.TryAddProperty(index, methods)
            then Ok index
            else ExistingPropertyMethodsError index :> ValidationError |> Error
        | ValueNone -> DuplicatePropertyError row :> ValidationError |> Error

    static member AddProperty<'Tag, 'Signature, 'Method>
        (
            builder,
            parent,
            property: Property<'Tag, 'Signature>,
            getter: RawIndex<'Method> voption,
            setter: RawIndex<'Method> voption
        ) =
        let getter' = ValueOption.map Unsafe.ChangeIndexTag<_, MethodDefRow> getter
        let setter' = ValueOption.map Unsafe.ChangeIndexTag<_, MethodDefRow> setter
        let methods = PropertyMethods(getter', setter', ImmutableArray.Empty)
        Unsafe.AddProperty(builder, parent, property, methods)

    static member AddInstanceProperty(builder, parent, property: InstanceProperty, getter, setter) =
        Unsafe.AddProperty<_, _, InstanceMethod>(builder, parent, property, getter, setter)
end

let private addClassDef (builder: CliMetadataBuilder) (def: ClassDef<'Flags>) =
    Unsafe.AddTypeDef<ClassDef<'Flags>>(
        builder,
        def.Flags.Value ||| def.Access.Tag,
        def.ClassName,
        def.TypeNamespace,
        def.Extends,
        TypeVisibility.enclosingClass def.Access
    )

[<Obsolete>]
[<RequireQualifiedAccess>]
module ConcreteClass =
    let addTypeDef builder (classDef: ConcreteClassDef): Result<RawIndex<ConcreteClassDef>, _> = addClassDef builder classDef

    let addInstanceMethod builder (owner: RawIndex<ConcreteClassDef>) (method: InstanceMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<InstanceMethod>, _>

    let addFinalMethod builder (owner: RawIndex<ConcreteClassDef>) (method: FinalMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<FinalMethod>, _>

    let addStaticMethod builder (owner: RawIndex<ConcreteClassDef>) (method: StaticMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<StaticMethod>, _>

    let addConstructor builder (owner: RawIndex<ConcreteClassDef>) (method: ObjectConstructor) =
        Unsafe.AddConstructor(builder, owner.AsTypeIndex(), method)

    let addClassConstructor builder (owner: RawIndex<ConcreteClassDef>) (method: ClassConstructor) =
        Unsafe.AddConstructor(builder, owner.AsTypeIndex(), method)

    let addEntryPoint builder (owner: RawIndex<ConcreteClassDef>) (method: EntryPointMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<EntryPointMethod>, _>

    let addInstanceField builder (owner: RawIndex<ConcreteClassDef>) (field: InstanceField) =
        Unsafe.AddField<_>(builder, owner.AsTypeIndex(), field): Result<RawIndex<InstanceField>, _>

    let addStaticField builder (owner: RawIndex<ConcreteClassDef>) (field: StaticField) =
        Unsafe.AddField<_>(builder, owner.AsTypeIndex(), field): Result<RawIndex<StaticField>, _>

[<Obsolete>]
[<RequireQualifiedAccess>]
module AbstractClass =
    let addTypeDef builder (classDef: AbstractClassDef): Result<RawIndex<AbstractClassDef>, _> = addClassDef builder classDef

    let addInstanceMethod builder (owner: RawIndex<AbstractClassDef>) (method: InstanceMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<InstanceMethod>, _>

    let addAbstractMethod builder (owner: RawIndex<AbstractClassDef>) (method: AbstractMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<AbstractMethod>, _>

    let addFinalMethod builder (owner: RawIndex<AbstractClassDef>) (method: FinalMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<FinalMethod>, _>

    let addStaticMethod builder (owner: RawIndex<AbstractClassDef>) (method: StaticMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<StaticMethod>, _>

    let addConstructor builder (owner: RawIndex<AbstractClassDef>) (method: ObjectConstructor) =
        Unsafe.AddConstructor(builder, owner.AsTypeIndex(), method)

    let addClassConstructor builder (owner: RawIndex<AbstractClassDef>) (method: ClassConstructor) =
        Unsafe.AddConstructor(builder, owner.AsTypeIndex(), method)

    let addEntryPoint builder (owner: RawIndex<AbstractClassDef>) (method: EntryPointMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<EntryPointMethod>, _>

    let addInstanceField builder (owner: RawIndex<AbstractClassDef>) (field: InstanceField) =
        Unsafe.AddField<_>(builder, owner.AsTypeIndex(), field): Result<RawIndex<InstanceField>, _>

    let addStaticField builder (owner: RawIndex<AbstractClassDef>) (field: StaticField) =
        Unsafe.AddField<_>(builder, owner.AsTypeIndex(), field): Result<RawIndex<StaticField>, _>

[<Obsolete>]
[<RequireQualifiedAccess>]
module SealedClass =
    let addTypeDef builder (classDef: SealedClassDef): Result<RawIndex<SealedClassDef>, _> = addClassDef builder classDef

    let addInstanceMethod builder (owner: RawIndex<SealedClassDef>) (method: InstanceMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<InstanceMethod>, _>

    let addFinalMethod builder (owner: RawIndex<SealedClassDef>) (method: FinalMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<FinalMethod>, _>

    let addStaticMethod builder (owner: RawIndex<SealedClassDef>) (method: StaticMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<StaticMethod>, _>

    let addConstructor builder (owner: RawIndex<SealedClassDef>) (method: ObjectConstructor) =
        Unsafe.AddConstructor(builder, owner.AsTypeIndex(), method)

    let addClassConstructor builder (owner: RawIndex<SealedClassDef>) (method: ClassConstructor) =
        Unsafe.AddConstructor(builder, owner.AsTypeIndex(), method)

    let addEntryPoint builder (owner: RawIndex<SealedClassDef>) (method: EntryPointMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<EntryPointMethod>, _>

    let addInstanceField builder (owner: RawIndex<SealedClassDef>) (field: InstanceField) =
        Unsafe.AddField<_>(builder, owner.AsTypeIndex(), field): Result<RawIndex<InstanceField>, _>

    let addStaticField builder (owner: RawIndex<SealedClassDef>) (field: StaticField) =
        Unsafe.AddField<_>(builder, owner.AsTypeIndex(), field): Result<RawIndex<StaticField>, _>

[<Obsolete>]
[<RequireQualifiedAccess>]
module StaticClass =
    let addTypeDef builder (classDef: StaticClassDef): Result<RawIndex<StaticClassDef>, _> = addClassDef builder classDef

    let addStaticMethod builder (owner: RawIndex<StaticClassDef>) (method: StaticMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<StaticMethod>, _>

    let addClassConstructor builder (owner: RawIndex<StaticClassDef>) (method: ClassConstructor) =
        Unsafe.AddConstructor(builder, owner.AsTypeIndex(), method)

    let addEntryPoint builder (owner: RawIndex<StaticClassDef>) (method: EntryPointMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<EntryPointMethod>, _>

    let addInstanceField builder (owner: RawIndex<StaticClassDef>) (field: InstanceField) =
        Unsafe.AddField<_>(builder, owner.AsTypeIndex(), field): Result<RawIndex<InstanceField>, _>

    let addStaticField builder (owner: RawIndex<StaticClassDef>) (field: StaticField) =
        Unsafe.AddField<_>(builder, owner.AsTypeIndex(), field): Result<RawIndex<StaticField>, _>

//module Delegate =
//module Enum =

[<Obsolete>]
[<RequireQualifiedAccess>]
module Interface =
    let addTypeDef builder (intfDef: InterfaceDef) =
        Unsafe.AddTypeDef<InterfaceDef>(
            builder,
            intfDef.Access.Tag ||| TypeAttributes.Interface ||| TypeAttributes.Abstract,
            intfDef.InterfaceName,
            intfDef.TypeNamespace,
            Extends.Null,
            TypeVisibility.enclosingClass intfDef.Access
        )

    let addAbstractMethod builder (owner: RawIndex<InterfaceDef>) (method: AbstractMethod) =
        Unsafe.AddMethod(builder, owner.ChangeTag(), method): Result<RawIndex<AbstractMethod>, _>

[<Obsolete>]
[<RequireQualifiedAccess>]
module Struct =
    let addTypeDef builder (lookup: TypeLookupCache) (structDef: StructDef) =
        let valueType = "System", Identifier.ofStr "ValueType"
        match lookup.TryFindType valueType with
        | ValueSome (TypeLookupResult.TypeRef tref) -> Unsafe.AddStruct(builder, tref, structDef)
        | ValueSome (TypeLookupResult.TypeDef tdef) -> Unsafe.AddStruct(builder, tdef, structDef)
        | ValueNone -> MissingTypeError(valueType).ToResult()

    let addInstanceMethod builder (owner: RawIndex<StructDef>) (method: InstanceMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<InstanceMethod>, _>

    let addStaticMethod builder (owner: RawIndex<StructDef>) (method: StaticMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<StaticMethod>, _>

    let addConstructor builder (owner: RawIndex<StructDef>) (method: ObjectConstructor) =
        Unsafe.AddConstructor(builder, owner.AsTypeIndex(), method)

    let addClassConstructor builder (owner: RawIndex<StructDef>) (method: ClassConstructor) =
        Unsafe.AddConstructor(builder, owner.AsTypeIndex(), method)

    let addEntryPoint builder (owner: RawIndex<StructDef>) (method: EntryPointMethod) =
        Unsafe.AddMethod(builder, owner.AsTypeIndex(), method): Result<RawIndex<EntryPointMethod>, _>

    let addInstanceField builder (owner: RawIndex<StructDef>) (field: InstanceField) =
        Unsafe.AddField<_>(builder, owner.AsTypeIndex(), field): Result<RawIndex<InstanceField>, _>

    let addStaticField builder (owner: RawIndex<StructDef>) (field: StaticField) =
        Unsafe.AddField<_>(builder, owner.AsTypeIndex(), field): Result<RawIndex<StaticField>, _>

    // let implementDef

    let implementRef builder (structDef: RawIndex<StructDef>) (intf: RawIndex<TypeRef>) =
        Unsafe.ImplementInterface(builder, structDef.AsTypeIndex(), InterfaceIndex.TypeRef intf)

    let implementSpec builder (structDef: RawIndex<StructDef>) (spec: RawIndex<TypeSpecRow>) =
        Unsafe.ImplementInterface(builder, structDef.AsTypeIndex(), InterfaceIndex.TypeSpec spec)

// TODO: Add functions for adding global fields and global methods.

[<Obsolete>]
let referenceType (builder: CliMetadataBuilder) typeRef =
    match builder.TypeRef.TryAdd typeRef with
    | ValueSome i -> Ok i
    | ValueNone -> DuplicateTypeRefError(typeRef).ToResult()

// TODO: Figure out how to remove warning checks while still having easy way to check if duplicate value was added.

[<Obsolete>]
/// <summary>Adds a reference to a method with the <c>DEFAULT</c> calling convention.</summary>
let referenceDefaultMethod (builder: CliMetadataBuilder) (method: MethodRefDefault) =
    builder.MemberRef.Add &method

[<Obsolete>]
/// <summary>Adds a reference to a method with the <c>GENERIC</c> calling convention.</summary>
let referenceGenericMethod (builder: CliMetadataBuilder) (method: MethodRefGeneric): struct(RawIndex<MethodRefGeneric> * _) =
    builder.MemberRef.Add &method

[<Obsolete>]
/// <summary>Adds a reference to a method with the <c>VARARG</c> calling convention.</summary>
let referenceVarArgMethod (builder: CliMetadataBuilder) (method: MethodRefVarArg): struct(RawIndex<MethodRefVarArg> * _) =
    builder.MemberRef.Add &method

[<Obsolete>]
let referenceModule (builder: CliMetadataBuilder) moduleRef = builder.ModuleRef.Add &moduleRef

[<Obsolete>]
/// Adds a reference to an assembly.
let referenceAssembly (builder: CliMetadataBuilder) assembly =
    let mutable dup = false
    let i = builder.AssemblyRef.Add(&assembly, &dup)
    struct(i, dup)

let addTypeSpec (builder: CliMetadataBuilder) typeSpec =
    let row = TypeSpecRow typeSpec
    match builder.TypeSpec.TryAdd row with
    | ValueSome index -> Ok index
    | ValueNone -> DuplicateTypeSpecError(row).ToResult()

[<Obsolete>]
let addMethodSpec (builder: CliMetadataBuilder) method (garguments: seq<_>) =
    let inst =
        MethodSpec garguments
        |> builder.Blobs.MethodSpec.TryAdd
        |> Result.any
    let spec' = MethodSpecRow(method, inst)
    match builder.MethodSpec.TryAdd spec' with
    | ValueSome index -> Ok index
    | ValueNone -> DuplicateMethodSpecError(spec').ToResult()

// TODO: Move functions for manipulating generic parameters to Unsafe.
[<RequireQualifiedAccess>]
module GenericParam =
    let addNonvariant (builder: CliMetadataBuilder) flags owner name constraints =
        let result = builder.GenericParam.TryAddNonvariant(flags, owner, name, constraints)
        match result with
        | ValueSome info -> Ok info
        | ValueNone -> DuplicateGenericParamError(owner ,name).ToResult()
