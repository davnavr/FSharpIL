/// <summary>
/// Contains functions for modifying the CLI metadata without CLS checks and warnings.
/// </summary>
module FSharpIL.Metadata.Unchecked

open System
open System.Collections.Immutable

// TODO: Figure out if using inref with static methods in Unsafe is a performance improvement.
/// Contains static methods for modifying the CLI metadata without regard for generation of correct metadata.
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

    static member AddMethod<'Body, 'Flags, 'Signature when 'Body :> IMethodBody and 'Signature :> IMethodDefSignature>
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
            method.Signature.Signature()
        )

    static member AddConstructor(builder: CliMetadataBuilder, owner, method: ClassConstructor) =
        Unsafe.AddConstructor<ClassConstructor, ClassConstructorTag, _> (
            builder,
            owner,
            method,
            ".cctor",
            Unsafe.ClassConstructorSignature
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

    static member private AddStruct(builder, valueType, structDef: StructDef) =
        Unsafe.AddTypeDef<StructDef>(
            builder,
            structDef.Flags.Value ||| structDef.Access.Tag,
            structDef.StructName,
            structDef.TypeNamespace,
            valueType,
            TypeVisibility.enclosingClass structDef.Access
        )

    static member AddStruct(builder, valueType, structDef: StructDef) =
        Unsafe.AddStruct(builder, Extends.TypeRef valueType, structDef)

    static member AddStruct(builder, valueType: RawIndex<TypeDefRow>, structDef: StructDef) =
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

    static member AddProperty<'Tag, 'Signature, 'Method when 'Signature :> IPropertySignature>
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
//module Interface =

[<RequireQualifiedAccess>]
module Struct =
    let addTypeDef builder (lookup: TypeLookupCache) (structDef: StructDef) =
        let valueType = "System", Identifier.ofStr "ValueType"
        match lookup.FindType valueType with
        | ValueSome (TypeLookupResult.TypeRef tref) -> Unsafe.AddStruct(builder, tref, structDef)
        | ValueSome (TypeLookupResult.TypeDef tdef) -> Unsafe.AddStruct(builder, tdef, structDef)
        | ValueNone -> MissingTypeError valueType :> ValidationError |> Error

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

/// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
let referenceType (builder: CliMetadataBuilder) typeRef =
    match builder.TypeRef.TryAdd typeRef with
    | ValueSome i -> Ok i
    | ValueNone -> DuplicateTypeRefError typeRef :> ValidationError |> Error

// TODO: Figure out how to remove warning checks while still having easy way to check if duplicate value was added.

let internal referenceMethod (builder: CliMetadataBuilder) (row: MemberRefRow) =
    builder.MemberRef.Add row

/// <summary>Adds a reference to a method with the <c>DEFAULT</c> calling convention.</summary>
let referenceDefaultMethod builder method: struct(RawIndex<MethodRefDefault> * bool) =
    referenceMethod builder (MethodRefDefault method)

/// <summary>Adds a reference to a method with the <c>GENERIC</c> calling convention.</summary>
let referenceGenericMethod builder method: struct(RawIndex<MethodRefGeneric> * _) =
    referenceMethod builder (MethodRefGeneric method)

/// <summary>Adds a reference to a method with the <c>VARARG</c> calling convention.</summary>
let referenceVarArgMethod builder method: struct(RawIndex<MethodRefVarArg> * _) =
    referenceMethod builder (MethodRefVarArg method)

let referenceModule (builder: CliMetadataBuilder) moduleRef = builder.ModuleRef.Add moduleRef

/// Adds a reference to an assembly.
let referenceAssembly (builder: CliMetadataBuilder) assembly =
    let mutable dup = false
    let i = builder.AssemblyRef.Add(assembly, &dup)
    struct(i, dup)

let addTypeSpec (builder: CliMetadataBuilder) typeSpec =
    match builder.TypeSpec.TryAdd typeSpec with
    | ValueSome index -> Ok index
    | ValueNone -> DuplicateTypeSpecError typeSpec :> ValidationError |> Error

let addMethodSpec (builder: CliMetadataBuilder) method (garguments: seq<_>) =
    let spec' = MethodSpecRow(method, MethodSpec garguments)
    match builder.MethodSpec.TryAdd spec' with
    | ValueSome index -> Ok index
    | ValueNone -> DuplicateMethodSpecError spec' :> ValidationError |> Error

// TODO: Move functions for manipulating generic parameters to Unsafe.
[<RequireQualifiedAccess>]
module GenericParam =
    let addNonvariant (builder: CliMetadataBuilder) flags owner name constraints =
        let result = builder.GenericParam.TryAddNonvariant(flags, owner, name, constraints)
        match result with
        | ValueSome info -> Ok info
        | ValueNone -> DuplicateGenericParamError(owner ,name) :> ValidationError |> Error
