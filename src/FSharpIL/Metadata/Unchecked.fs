/// <summary>
/// Contains functions for modifying the CLI metadata without CLS checks and warnings.
/// </summary>
module FSharpIL.Metadata.Unchecked

open System
open System.Collections.Immutable

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
        IndexOwner.checkOwner builder.Owner field
        match builder.Field.Add(owner, field) with
        | ValueSome index -> FieldIndex<'Tag> index |> Result<FieldIndex<_>, _>.Ok
        | ValueNone -> DuplicateFieldError field :> ValidationError |> Error

    static member internal AddField<'Flags>(builder, owner, field: Field<'Flags>) =
        Unsafe.AddField<Field<'Flags>>(builder, owner, field.Row())

    static member internal AddMethod<'Tag>(builder: CliMetadataBuilder, owner, method) =
        IndexOwner.checkOwner builder.Owner method
        match builder.Method.Add(owner, method) with
        | ValueSome index -> MethodDefIndex<'Tag> index |> Result<MethodDefIndex<_>, _>.Ok
        | ValueNone -> DuplicateMethodError method :> ValidationError |> Error

    static member AddMethod<'Body, 'Flags, 'Signature when 'Body :> IMethodBody and 'Signature :> IMethodDefSignature>
        (
            builder: CliMetadataBuilder,
            owner,
            method: Method<'Body, 'Flags, 'Signature>
        ) =
        Unsafe.AddMethod<Method<'Body, 'Flags, 'Signature>>(builder, owner, method.Definition())

    static member private AddConstructor<'Tag, 'Signature>
        (
            builder: CliMetadataBuilder,
            owner,
            method: Constructor<'Tag, 'Signature>,
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
        IndexOwner.checkOwner builder.Owner row
        Unsafe.AddMethod<'Tag>(builder, owner, row)

    static member AddConstructor(builder: CliMetadataBuilder, owner, method: ObjectConstructor) =
        Unsafe.AddConstructor<ObjectConstructorTag, _>(builder, owner, method, ".ctor", method.Signature.Signature())

    static member AddConstructor(builder: CliMetadataBuilder, owner, method: ClassConstructor) =
        Unsafe.AddConstructor<ClassConstructorTag, _>(builder, owner, method, ".cctor", Unsafe.ClassConstructorSignature)

    static member AddTypeDef<'Tag>
        (
            builder: CliMetadataBuilder,
            flags,
            typeName,
            typeNamespace,
            extends,
            parent
        ): Result<TypeDefIndex<'Tag>, _> =
        let row =
            TypeDefRow (
                flags,
                typeName,
                typeNamespace,
                extends,
                parent
            )
        match builder.TypeDef.TryAdd row with
        | ValueSome index -> TypeDefIndex<'Tag> index |> Ok
        | ValueNone -> DuplicateTypeDefError row :> ValidationError |> Error

    static member AddTypeDef<'Tag>(builder, flags, typeName, extends) =
        Unsafe.AddTypeDef<'Tag>(builder, flags, typeName, String.Empty, extends, None)

    static member private AddStruct(builder, valueType, structDef: StructDef) =
        Unsafe.AddTypeDef<StructDef>(
            builder,
            structDef.Flags.Value ||| structDef.Access.Flags,
            structDef.StructName,
            structDef.TypeNamespace,
            valueType,
            structDef.Access.EnclosingClass
        )

    static member AddStruct(builder, valueType, structDef: StructDef) =
        Unsafe.AddStruct(builder, Extends.TypeRef valueType, structDef)

    static member AddStruct(builder, valueType: SimpleIndex<TypeDefRow>, structDef: StructDef) =
        Unsafe.AddStruct(builder, (invalidOp "TODO: Add Extends.TypeDef to allow addition of structs that derive from System.ValueType in the same assembly": Extends), structDef)

    static member ChangeFlagTag<'Flags, 'From, 'To when 'Flags :> Enum>(flags: ValidFlags<'From, 'Flags>) =
        ValidFlags<'To, 'Flags> flags.Value

    static member ChangeIndexTag<'Value, 'From, 'To when 'Value : equality>(index: TaggedIndex<'From, 'Value>) =
        TaggedIndex<'To, 'Value> index.Index

    static member CreateFlags<'Tag, 'Flags when 'Flags :> Enum> flags = ValidFlags<'Tag, 'Flags> flags
end

let private addClassDef (builder: CliMetadataBuilder) (def: ClassDef<'Flags>) =
    Unsafe.AddTypeDef<ClassDef<'Flags>>(
        builder,
        def.Flags.Value ||| def.Access.Flags,
        def.ClassName,
        def.TypeNamespace,
        def.Extends,
        def.Access.EnclosingClass
    )

[<RequireQualifiedAccess>]
module ConcreteClass =
    let addTypeDef builder (classDef: ConcreteClassDef): Result<TypeDefIndex<ConcreteClassDef>, _> = addClassDef builder classDef

    let addInstanceMethod builder (owner: TypeDefIndex<ConcreteClassDef>) (method: InstanceMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<InstanceMethod>, _>

    let addFinalMethod builder (owner: TypeDefIndex<ConcreteClassDef>) (method: FinalMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<FinalMethod>, _>

    let addStaticMethod builder (owner: TypeDefIndex<ConcreteClassDef>) (method: StaticMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<StaticMethod>, _>

    let addConstructor builder (owner: TypeDefIndex<ConcreteClassDef>) (method: ObjectConstructor) =
        Unsafe.AddConstructor(builder, owner.Index, method)

    let addClassConstructor builder (owner: TypeDefIndex<ConcreteClassDef>) (method: ClassConstructor) =
        Unsafe.AddConstructor(builder, owner.Index, method)

    let addEntryPoint builder (owner: TypeDefIndex<ConcreteClassDef>) (method: EntryPointMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<EntryPointMethod>, _>

    let addInstanceField builder (owner: TypeDefIndex<ConcreteClassDef>) (field: InstanceField) =
        Unsafe.AddField<_>(builder, owner.Index, field): Result<FieldIndex<InstanceField>, _>

    let addStaticField builder (owner: TypeDefIndex<ConcreteClassDef>) (field: StaticField) =
        Unsafe.AddField<_>(builder, owner.Index, field): Result<FieldIndex<StaticField>, _>

[<RequireQualifiedAccess>]
module AbstractClass =
    let addTypeDef builder (classDef: AbstractClassDef): Result<TypeDefIndex<AbstractClassDef>, _> = addClassDef builder classDef

    let addInstanceMethod builder (owner: TypeDefIndex<AbstractClassDef>) (method: InstanceMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<InstanceMethod>, _>

    let addAbstractMethod builder (owner: TypeDefIndex<AbstractClassDef>) (method: AbstractMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<AbstractMethod>, _>

    let addFinalMethod builder (owner: TypeDefIndex<AbstractClassDef>) (method: FinalMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<FinalMethod>, _>

    let addStaticMethod builder (owner: TypeDefIndex<AbstractClassDef>) (method: StaticMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<StaticMethod>, _>

    let addConstructor builder (owner: TypeDefIndex<AbstractClassDef>) (method: ObjectConstructor) =
        Unsafe.AddConstructor(builder, owner.Index, method)

    let addClassConstructor builder (owner: TypeDefIndex<AbstractClassDef>) (method: ClassConstructor) =
        Unsafe.AddConstructor(builder, owner.Index, method)

    let addEntryPoint builder (owner: TypeDefIndex<AbstractClassDef>) (method: EntryPointMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<EntryPointMethod>, _>

    let addInstanceField builder (owner: TypeDefIndex<AbstractClassDef>) (field: InstanceField) =
        Unsafe.AddField<_>(builder, owner.Index, field): Result<FieldIndex<InstanceField>, _>

    let addStaticField builder (owner: TypeDefIndex<AbstractClassDef>) (field: StaticField) =
        Unsafe.AddField<_>(builder, owner.Index, field): Result<FieldIndex<StaticField>, _>

[<RequireQualifiedAccess>]
module SealedClass =
    let addTypeDef builder (classDef: SealedClassDef): Result<TypeDefIndex<SealedClassDef>, _> = addClassDef builder classDef

    let addInstanceMethod builder (owner: TypeDefIndex<SealedClassDef>) (method: InstanceMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<InstanceMethod>, _>

    let addFinalMethod builder (owner: TypeDefIndex<SealedClassDef>) (method: FinalMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<FinalMethod>, _>

    let addStaticMethod builder (owner: TypeDefIndex<SealedClassDef>) (method: StaticMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<StaticMethod>, _>

    let addConstructor builder (owner: TypeDefIndex<SealedClassDef>) (method: ObjectConstructor) =
        Unsafe.AddConstructor(builder, owner.Index, method)

    let addClassConstructor builder (owner: TypeDefIndex<SealedClassDef>) (method: ClassConstructor) =
        Unsafe.AddConstructor(builder, owner.Index, method)

    let addEntryPoint builder (owner: TypeDefIndex<SealedClassDef>) (method: EntryPointMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<EntryPointMethod>, _>

    let addInstanceField builder (owner: TypeDefIndex<SealedClassDef>) (field: InstanceField) =
        Unsafe.AddField<_>(builder, owner.Index, field): Result<FieldIndex<InstanceField>, _>

    let addStaticField builder (owner: TypeDefIndex<SealedClassDef>) (field: StaticField) =
        Unsafe.AddField<_>(builder, owner.Index, field): Result<FieldIndex<StaticField>, _>

[<RequireQualifiedAccess>]
module StaticClass =
    let addTypeDef builder (classDef: StaticClassDef): Result<TypeDefIndex<StaticClassDef>, _> = addClassDef builder classDef

    let addStaticMethod builder (owner: TypeDefIndex<StaticClassDef>) (method: StaticMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<StaticMethod>, _>

    let addClassConstructor builder (owner: TypeDefIndex<StaticClassDef>) (method: ClassConstructor) =
        Unsafe.AddConstructor(builder, owner.Index, method)

    let addEntryPoint builder (owner: TypeDefIndex<StaticClassDef>) (method: EntryPointMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<EntryPointMethod>, _>

    let addInstanceField builder (owner: TypeDefIndex<StaticClassDef>) (field: InstanceField) =
        Unsafe.AddField<_>(builder, owner.Index, field): Result<FieldIndex<InstanceField>, _>

    let addStaticField builder (owner: TypeDefIndex<StaticClassDef>) (field: StaticField) =
        Unsafe.AddField<_>(builder, owner.Index, field): Result<FieldIndex<StaticField>, _>

let private addDerivedTypeDef (lookup: TypeLookupCache) (builder: CliMetadataBuilder) extends def (typeDef: 'Type) =
    match lookup.FindType extends with
    | ValueSome extends' -> def builder extends' typeDef |> TypeDefIndex |> Ok
    | ValueNone -> MissingTypeError extends :> ValidationError |> Error

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

    let addInstanceMethod builder (owner: TypeDefIndex<StructDef>) (method: InstanceMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<InstanceMethod>, _>

    let addStaticMethod builder (owner: TypeDefIndex<StructDef>) (method: StaticMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<StaticMethod>, _>

    let addConstructor builder (owner: TypeDefIndex<StructDef>) (method: ObjectConstructor) =
        Unsafe.AddConstructor(builder, owner.Index, method)

    let addClassConstructor builder (owner: TypeDefIndex<StructDef>) (method: ClassConstructor) =
        Unsafe.AddConstructor(builder, owner.Index, method)

    let addEntryPoint builder (owner: TypeDefIndex<StructDef>) (method: EntryPointMethod) =
        Unsafe.AddMethod(builder, owner.Index, method): Result<MethodDefIndex<EntryPointMethod>, _>

    let addInstanceField builder (owner: TypeDefIndex<StructDef>) (field: InstanceField) =
        Unsafe.AddField<_>(builder, owner.Index, field): Result<FieldIndex<InstanceField>, _>

    let addStaticField builder (owner: TypeDefIndex<StructDef>) (field: StaticField) =
        Unsafe.AddField<_>(builder, owner.Index, field): Result<FieldIndex<StaticField>, _>

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
let referenceDefaultMethod builder method: struct(MemberRefIndex<MethodRefDefault> * bool) =
    referenceMethod builder (MethodRefDefault method)

/// <summary>Adds a reference to a method with the <c>GENERIC</c> calling convention.</summary>
let referenceGenericMethod builder method: struct(MemberRefIndex<MethodRefGeneric> * _) =
    referenceMethod builder (MethodRefGeneric method)

/// <summary>Adds a reference to a method with the <c>VARARG</c> calling convention.</summary>
let referenceVarArgMethod builder method: struct(MemberRefIndex<MethodRefVarArg> * _) =
    referenceMethod builder (MethodRefVarArg method)

let referenceModule (builder: CliMetadataBuilder) moduleRef = builder.ModuleRef.Add moduleRef

/// Adds a reference to an assembly.
let referenceAssembly (builder: CliMetadataBuilder) assembly = builder.AssemblyRef.Add assembly

let addTypeSpec (builder: CliMetadataBuilder) typeSpec =
    match builder.TypeSpec.TryAdd typeSpec with
    | ValueSome index -> Ok index
    | ValueNone -> DuplicateTypeSpecError typeSpec :> ValidationError |> Error

let addMethodSpec (builder: CliMetadataBuilder) method (garguments: seq<_>) =
    let spec' = MethodSpecRow(method, MethodSpec garguments)
    match builder.MethodSpec.TryAdd spec' with
    | ValueSome index -> Ok index
    | ValueNone -> DuplicateMethodSpecError spec' :> ValidationError |> Error
