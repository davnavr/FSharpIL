/// <summary>
/// Contains functions for modifying the CLI metadata without CLS checks and warnings.
/// </summary>
module FSharpIL.Metadata.Unchecked

open System
open System.Collections.Immutable

/// Contains static methods for modifying the CLI metadata without regard for generation of correct metadata.
[<AbstractClass; Sealed>]
type Unsafe = class
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

    static member AddStruct(builder, valueType, structDef: StructDef) =
        Unsafe.AddTypeDef<StructDef>(
            builder,
            structDef.Flags.Value ||| structDef.Access.Flags, // TODO: Call Flags.structFlags here.
            structDef.StructName,
            structDef.TypeNamespace,
            Extends.TypeRef valueType,
            structDef.Access.EnclosingClass
        )

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

let addClass builder (classDef: ConcreteClassDef): Result<TypeDefIndex<ConcreteClassDef>, _> = addClassDef builder classDef
let addAbstractClass builder (classDef: AbstractClassDef): Result<TypeDefIndex<AbstractClassDef>, _> =
    addClassDef builder classDef
let addSealedClass builder (classDef: SealedClassDef): Result<TypeDefIndex<SealedClassDef>, _> = addClassDef builder classDef
let addStaticClass builder (classDef: StaticClassDef): Result<TypeDefIndex<StaticClassDef>, _> = addClassDef builder classDef

let private addDerivedTypeDef (lookup: TypeLookupCache) (builder: CliMetadataBuilder) extends def (typeDef: 'Type) =
    match lookup.FindType extends with
    | ValueSome extends' -> def builder extends' typeDef |> TypeDefIndex |> Ok
    | ValueNone -> MissingTypeError extends :> ValidationError |> Error

// let addDelegate
// let addEnum
// let addInterface
let addStruct builder (lookup: TypeLookupCache) = failwith "TODO: Add struct after finding ValueType"

let addField
    (builder: CliMetadataBuilder)
    (SimpleIndex owner: TypeDefIndex<'Type>)
    (FieldRow field: 'Field when 'Field :> IField<'Type>)
    =
    match builder.Field.Add(owner, field) with
    | ValueSome index -> FieldIndex<'Field> index |> Result<FieldIndex<_>, _>.Ok
    | ValueNone -> DuplicateFieldError field :> ValidationError |> Error

let addMethod
    (builder: CliMetadataBuilder)
    (SimpleIndex owner: TypeDefIndex<'Type>)
    (MethodDef method: 'Method when 'Method :> IMethod<'Type>)
    =
    match builder.Method.Add(owner, method) with
    | ValueSome index -> MethodDefIndex<'Method> index |> Result<MethodDefIndex<_>, _>.Ok
    | ValueNone -> DuplicateMethodError method :> ValidationError |> Error

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
