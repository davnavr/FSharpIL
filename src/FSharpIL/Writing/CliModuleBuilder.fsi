﻿namespace FSharpIL.Writing

open System
open System.Collections.Generic
open System.Runtime.CompilerServices

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

open FSharpIL.Writing.Cil

open FSharpIL.Utilities.Collections

[<NoComparison; NoEquality>]
type EntryPoint

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type CustomAttributeList =
    member Count: int32
    member Add: CustomAttribute -> IValidationError option

/// Used to obtain a mutable list of custom attributes.
type CustomAttributeBuilder = CustomAttributeList ref voption

/// Used to generate a method for a property or event.
type MethodGenerator = DefinedMethod * MethodBody voption * CustomAttributeBuilder

[<Sealed>]
type DefinedTypeMembers =
    member Owner: DefinedType
    member FieldCount: int32
    member MethodCount: int32
    member PropertyCount: int32
    member EventCount: int32

    member DefineField:
        field: DefinedField *
        attributes: CustomAttributeBuilder -> ValidationResult<FieldTok<DefinedType, DefinedField>>

    member DefineMethod:
        method: DefinedMethod *
        body: MethodBody voption *
        attributes: CustomAttributeBuilder -> ValidationResult<MethodTok<DefinedType, DefinedMethod>>

    member DefineEntryPoint:
        method: EntryPointMethod *
        body: MethodBody *
        attributes: CustomAttributeBuilder -> ValidationResult<MethodTok<DefinedType, MethodDefinition<MethodKinds.Static>>>

    member DefineProperty:
        name: Identifier *
        getter: MethodGenerator voption *
        setter: MethodGenerator voption *
        other: MethodGenerator list *
        attributes: CustomAttributeBuilder -> ValidationResult<PropertyTok>

    member DefineEvent:
        name: Identifier *
        etype: TypeTok *
        add: MethodGenerator *
        remove: MethodGenerator *
        raise: MethodGenerator voption *
        other: MethodGenerator list *
        attributes: CustomAttributeBuilder -> ValidationResult<EventTok>

    member ContainsField: field: DefinedField -> bool
    member ContainsMethod: method: DefinedMethod -> bool

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type DefinedTypeMembers<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct> =
    val Members: DefinedTypeMembers

[<Sealed>]
type ReferencedTypeMembers =
    [<DefaultValue>] val mutable internal Field: HybridHashSet<ReferencedField>
    [<DefaultValue>] val mutable internal Method: HybridHashSet<ReferencedMethod>

    member Owner: ReferencedType
    member FieldCount: int32
    member MethodCount: int32
    //member PropertyCount: int32
    //member EventCount: int32

    member ReferenceMethod: method: ReferencedMethod -> ValidationResult<MethodTok<ReferencedType, ReferencedMethod>>

    member ContainsField: field: ReferencedField -> bool
    member ContainsMethod: method: ReferencedMethod -> bool

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type ReferencedTypeMembers<'Kind when 'Kind :> IAttributeTag<TypeDefFlags> and 'Kind : struct> =
    val Members: ReferencedTypeMembers

[<AbstractClass; Sealed; Extension>]
type TypeMemberExtensions =
    [<Extension>]
    static member ReferenceMethod :
        members: ReferencedTypeMembers<'Kind> *
        method: MethodReference<MethodKinds.ObjectConstructor> ->
            ValidationResult<MethodTok<TypeReference<'Kind>, MethodReference<MethodKinds.ObjectConstructor>>>
            when 'Kind :> TypeAttributes.IHasStaticMethods

    [<Extension>]
    static member ReferenceMethod :
        members: ReferencedTypeMembers<'Kind> *
        method: MethodReference<MethodKinds.Static> ->
            ValidationResult<MethodTok<TypeReference<'Kind>, MethodReference<MethodKinds.Static>>>
            when 'Kind :> TypeAttributes.IHasStaticMethods

    [<Extension>]
    static member DefineMethod :
        members: DefinedTypeMembers<'Kind> *
        method: MethodDefinition<MethodKinds.ObjectConstructor> *
        body: MethodBody * // TODO: Define helper class for emitting call to base constructor before running rest of ctor code.
        attributes: CustomAttributeBuilder ->
            ValidationResult<MethodTok<TypeDefinition<'Kind>, MethodDefinition<MethodKinds.ObjectConstructor>>>
            when 'Kind :> TypeKinds.IHasConstructors

    [<Extension>]
    static member DefineMethod :
        members: DefinedTypeMembers<'Kind> *
        method: MethodDefinition<MethodKinds.Instance> *
        body: MethodBody *
        attributes: CustomAttributeBuilder ->
            ValidationResult<MethodTok<TypeDefinition<'Kind>, MethodDefinition<MethodKinds.Instance>>>
            when 'Kind :> TypeKinds.IHasInstanceMethods

    //static member DefineEntryPoint

/// Builds a CLI metadata module (I.9).
[<Sealed>]
type CliModuleBuilder =
    /// <summary>Creates a new CLI metadata module.</summary>
    /// <param name="name">The name of the module.</param>
    /// <param name="mvid">
    /// Used to uniquely identify the module, can be generated by any algorithim that generates new GUIDs or can be based off of
    /// the contents of the module for deterministic outputs.
    /// </param>
    /// <param name="cliMetadataHeader">
    /// Specifies the version of the runtime the module can run on and if the module can only run on a 32-bit machine, defaults to
    /// the latest runtime version if omitted.
    /// </param>
    /// <param name="cliMetadataRoot">
    /// Specifies which implementation of the Common Language Runtime the module is intended to be run on, defaults to the latest
    /// Microsoft-specific implementation if omitted.
    /// </param>
    /// <param name="assembly">
    /// Defines an assembly manifest, if omitted then the resulting Portable Executable file is not an assembly.
    /// </param>
    /// <param name="warnings">
    /// A collection to which warning objects are added to, if omitted any <c>WARNING</c> checks are ignored.
    /// </param>
    /// <param name="typeDefCapacity">The initial number of type definitions that the module can contain.</param>
    /// <param name="typeRefCapacity">The initial number of type references that the module can contain.</param>
    /// <param name="assemblyRefCapacity">The initial number of assembly references that the module can contain.</param>
    new :
        name: Identifier *
        ?mvid: Guid *
        ?cliMetadataHeader: CliHeader *
        ?cliMetadataRoot: CliMetadataRoot<FSharpIL.Omitted, FSharpIL.Omitted> *
        ?assembly: DefinedAssembly *
        ?warnings: ValidationWarningsBuilder *
        ?typeDefCapacity: int32 *
        ?typeRefCapacity: int32 *
        ?assemblyRefCapacity: int32 -> CliModuleBuilder

    member Mvid: Guid
    member Name: Identifier
    member ModuleCustomAttributes: CustomAttributeList
    member Assembly: DefinedAssembly option
    member AssemblyCustomAttributes: CustomAttributeList option
    member EntryPoint: EntryPoint // TODO: Allow a File row to also be an entry point.
    member ValidationWarnings: ValidationWarningsCollection
    /// <summary>
    /// Gets the members of the <c>&lt;Module&gt;</c> special type, which represents the global members defined in the module.
    /// </summary>
    member GlobalMembers: DefinedTypeMembers

    member DefineAssembly: assembly: DefinedAssembly -> ValidationResult<CustomAttributeList>

    /// Adds a reference to another assembly.
    member ReferenceAssembly: assembly: ReferencedAssembly -> unit

    // TODO: For methods that add things that can also have custom attributes, figure out how to avoid allocating a CustomAttributeList if user doesn't want/need the CA list.

    member DefineType: definition: DefinedType -> ValidationResult<struct(CustomAttributeList * DefinedTypeMembers)>

    member DefineType:
        definition: DefinedType *
        attributes: CustomAttributeBuilder -> ValidationResult<DefinedTypeMembers>

    //member DefineType: DefinedType * attributes: outref<CustomAttributeList> -> ValidationResult<DefinedTypeMembers>

    member DefineType:
        definition: TypeDefinition<'Kind> *
        attributes: CustomAttributeBuilder -> ValidationResult<DefinedTypeMembers<'Kind>>

    member DefineGenericType:
        definition: GenericType<TypeDefinition> *
        attributes: CustomAttributeBuilder -> ValidationResult<DefinedTypeMembers>

    member DefineGenericType:
        definition: GenericType.Definition<'Kind> *
        attributes: CustomAttributeBuilder -> ValidationResult<DefinedTypeMembers<'Kind>>

    member ReferenceType: reference: ReferencedType -> ValidationResult<ReferencedTypeMembers>

    member ReferenceType: reference: TypeReference<'Kind> -> ValidationResult<ReferencedTypeMembers<'Kind>>

    member GenericInstantiation:
        isValueType: bool * // TODO: Use union type for this.
        field: FieldTok *
        typeGenericParameters: GenericType.ArgumentInitializer -> FieldTok

    member GenericInstantiation: // TODO: Make overload that allows setting of generic parameters of the method.
        isValueType: bool *
        method: MethodTok *
        typeGenericParameters: GenericType.ArgumentInitializer -> MethodTok

    // TODO: Add helper methods for making Extends instances, maybe even replace public constructors for ClassExtends.

    member SetFieldRva: field: DefinedField * data: ReadOnlyMemory<byte> -> unit

    /// <summary>
    /// Attempts to add a <see cref="T:System.Runtime.Versioning.TargetFrameworkAttribute"/> to the current assembly.
    /// </summary>
    /// <returns>
    /// <c>None</c> if the attribute was successfully added; otherwise an error object if the current assembly is not defined or
    /// if the custom attribute constructor does not take exactly 1 argument.
    /// </returns>
    member SetTargetFramework: tfm: string * ctor: CustomAttributeCtor -> IValidationError option

    member internal CreateMetadata: unit -> CliMetadataBuilder
