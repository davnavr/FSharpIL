[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSharpIL.Metadata.CliMetadata

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

    // ChangeTag
end

// TODO: Better way of adding custom attributes, have a function: CustomAttribute -> target: _ -> MetadataBuilderState -> _
let addCustomAttribute (builder: CliMetadataBuilder) attr = builder.CustomAttribute.Add attr

/// <summary>
/// Sets the assembly information of the metadata, which specifies the version, name, and other information concerning the .NET
/// assembly.
/// </summary>
let setAssembly (builder: CliMetadataBuilder) assembly = builder.SetAssembly assembly

/// <summary>Sets the entrypoint of the assembly.</summary>
/// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
let setEntryPointToken (builder: CliMetadataBuilder) entryPoint = builder.SetEntryPointToken entryPoint

/// <summary>Sets the entrypoint of the assembly to a static method defined in the assembly.</summary>
/// <exception cref="T:FSharpIL.Metadata.IndexOwnerMismatchException"/>
let setEntryPoint builder main = setEntryPointToken builder (EntryPointToken.ValidEntryPoint main)

/// <summary>Adds a <c>TargetFrameworkAttribute</c> to the current assembly specifying the target framework.</summary>
/// <param name="builder" />
/// <param name="assembly">Dummy object used to guarantee that the CLI metadata is an assembly.</param>
/// <param name="ctor">The constructor for a <see cref="T:System.Runtime.Versioning.TargetFrameworkAttribute"/>.</param>
/// <param name="tfm">The target framework moniker. For .NET 5, the value is <c>.NETCoreApp,Version=v5.0</c>.</param>
let setTargetFramework builder (assembly: AssemblyIndex) (ctor: MemberRefIndex<_>) tfm =
    let tfm' = FixedArg.Elem (SerString tfm)
    // TODO: Check that the constructor is correct.
    { Parent = CustomAttributeParent.Assembly assembly
      Type = CustomAttributeType.MethodRefDefault ctor
      Value = Some { FixedArg = ImmutableArray.Create tfm'; NamedArg = ImmutableArray.Empty } }
    |> addCustomAttribute builder

// TODO: Come up with better name for modules. Maybe WarningsOnly, AllChecks, NoChecks, and RaiseOnError

/// Contains functions for modifying the CLI metadata with warnings only and no CLS checks.
module WarningChecked =
    // TODO: Enforce common warning checks for types.
    // TODO: Enforce warnings when adding a class.
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
    // let addStruct

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

    let private referenceMethod (builder: CliMetadataBuilder) (row: MemberRefRow) (warnings: WarningsBuilder) =
        let struct(i, duplicate) = builder.MemberRef.Add row
        if duplicate then warnings.Add(DuplicateMemberRefWarning row)
        i

    /// <summary>Adds a reference to a method with the <c>DEFAULT</c> calling convention.</summary>
    let referenceDefaultMethod builder method warnings: MemberRefIndex<MethodRefDefault> =
        referenceMethod builder (MethodRefDefault method) warnings

    /// <summary>Adds a reference to a method with the <c>GENERIC</c> calling convention.</summary>
    let referenceGenericMethod builder method warnings: MemberRefIndex<MethodRefGeneric> =
        referenceMethod builder (MethodRefGeneric method) warnings

    /// <summary>Adds a reference to a method with the <c>VARARG</c> calling convention.</summary>
    let referenceVarArgMethod builder method warnings: MemberRefIndex<MethodRefVarArg> =
        referenceMethod builder (MethodRefVarArg method) warnings

    let referenceModule (builder: CliMetadataBuilder) moduleRef (warnings: WarningsBuilder) =
        let i, dup = builder.ModuleRef.Add moduleRef
        if dup then warnings.Add(DuplicateModuleRefWarning moduleRef)
        i

    /// Adds a reference to an assembly.
    let referenceAssembly (builder: CliMetadataBuilder) assembly (warnings: WarningsBuilder) =
        let i, duplicate = builder.AssemblyRef.Add assembly
        if duplicate then warnings.Add(DuplicateAssemblyRefWarning assembly)
        i

    let addTypeSpec (builder: CliMetadataBuilder) typeSpec =
        match builder.TypeSpec.TryAdd typeSpec with
        | ValueSome index -> Ok index
        | ValueNone -> DuplicateTypeSpecError typeSpec :> ValidationError |> Error

    let addMethodSpec (builder: CliMetadataBuilder) method (garguments: seq<_>) =
        let spec' = MethodSpecRow(method, MethodSpec garguments)
        match builder.MethodSpec.TryAdd spec' with
        | ValueSome index -> Ok index
        | ValueNone -> DuplicateMethodSpecError spec' :> ValidationError |> Error

/// Contains functions for modifying the CLI metadata with CLS checks and warnings.
[<AutoOpen>]
module Checked =
    let addClass builder classDef = WarningChecked.addClass builder classDef
    let addAbstractClass builder classDef = WarningChecked.addAbstractClass builder classDef
    let addSealedClass builder classDef = WarningChecked.addSealedClass builder classDef
    let addStaticClass builder classDef = WarningChecked.addStaticClass builder classDef
    let addField builder owner field = WarningChecked.addField builder owner field
    let addMethod builder owner method = WarningChecked.addMethod builder owner method
    let referenceType builder typeRef = WarningChecked.referenceType builder typeRef
    let referenceDefaultMethod builder method warnings = WarningChecked.referenceDefaultMethod builder method warnings
    let referenceGenericMethod builder method warnings = WarningChecked.referenceGenericMethod builder method warnings
    let referenceVarArgMethod builder method warnings = WarningChecked.referenceVarArgMethod builder method warnings
    let referenceModule builder moduleRef warnings = WarningChecked.referenceModule builder moduleRef warnings
    let referenceAssembly builder assembly warnings = WarningChecked.referenceAssembly builder assembly warnings
    let addTypeSpec builder typeSpec = WarningChecked.addTypeSpec builder typeSpec
    let addMethodSpec builder method garguments = WarningChecked.addMethodSpec builder method garguments

/// <summary>
/// Contains functions for modifying the CLI metadata without CLS checks and warnings, throwing an exception on any errors.
/// </summary>
/// <remarks>
/// When an error is encountered, the functions in this module throw a <see cref="T:FSharpIL.Metadata.ValidationErrorException"/>.
/// </remarks>
module Unchecked =
    let private throwOnError =
        function
        | Ok item -> item
        | Error err -> raise(ValidationErrorException err)
    // TODO: Figure out how to skip warnings without allocating additional dummy array builders. Maybe call versions of functions that don't create warnings instead?
    let private skipWarnings(): WarningsBuilder = ImmutableArray.CreateBuilder<_> 0

    // TODO: Create version of Unsafe static class that throws exceptions on errors.

    let addClass builder (classDef: ConcreteClassDef) = WarningChecked.addClass builder classDef |> throwOnError
    let addAbstractClass builder (classDef: AbstractClassDef) = WarningChecked.addAbstractClass builder classDef |> throwOnError
    let addSealedClass builder (classDef: SealedClassDef) = WarningChecked.addSealedClass builder classDef |> throwOnError
    let addStaticClass builder (classDef: StaticClassDef) = WarningChecked.addStaticClass builder classDef |> throwOnError
    let addStruct builder (structDef: StructDef): TypeDefIndex<StructDef> = failwith "TODO: Figure out how to generate value types."
    let addField builder owner field = WarningChecked.addField builder owner field |> throwOnError
    let addMethod builder owner method = WarningChecked.addMethod builder owner method |> throwOnError
    let referenceType builder typeRef = WarningChecked.referenceType builder typeRef |> throwOnError
    // let referenceDefaultMethod // Throws no errors
    // let referenceGenericMethod
    // let referenceVarArgMethod
    // let referenceModule
    // let referenceAssembly
    let addTypeSpec builder typeSpec = WarningChecked.addTypeSpec builder typeSpec |> throwOnError
    let addMethodSpec builder method garguments = WarningChecked.addMethodSpec builder method garguments |> throwOnError

/// <summary>
/// Applies the given function to each string in the <c>#Strings</c> heap referenced in
/// the CLI metadata tables (II.24.2.6).
/// </summary>
let inline internal iterStrings action (metadata: CliMetadata) =
    string metadata.Module.Name |> action

    for tref in metadata.TypeRef.Rows do
        string tref.TypeName |> action
        action tref.TypeNamespace

    for tdef in metadata.TypeDef.Rows do
        string tdef.TypeName |> action
        action tdef.TypeNamespace

    for field in metadata.Field.Rows do
        string field.Name |> action

    for method in metadata.MethodDef.Rows do
        string method.Name |> action

    for _, param in metadata.Param do
        action param.ParamName



    for mref in metadata.MemberRef.Rows do
        match mref with
        | MethodRefDefault { MemberName = name }
        | MethodRefGeneric { MemberName = name }
        | MethodRefVarArg { MemberName = name } ->
            string name |> action

    // ModuleRef table not necessary, since its names will correspond to names used in the File table

    match metadata.Assembly with
    | ValueSome assembly ->
        string assembly.Name |> action
        string assembly.Culture |> action
    | ValueNone -> ()

    for assembly in metadata.AssemblyRef.Rows do
        string assembly.Name |> action
        string assembly.Culture |> action

    for { FileName = name } in metadata.File.Rows do
        string name |> action
