namespace FSharpIL.Writing

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Cil
open FSharpIL.Metadata.Tables

open FSharpIL.Writing.Cil

open FSharpIL.Utilities.Collections

[<AbstractClass>]
type DefinedMethodBody = // TODO: Maybe move MethodBodyBuilder higher up to allow its usage in the FSharpIL.Cli namespace.
    val InitLocals: InitLocals
    val LocalTypes: ImmutableArray<LocalVariableType>

    new: localTypes: ImmutableArray<LocalVariableType> * initLocals: InitLocals -> DefinedMethodBody
    new: localTypes: ImmutableArray<LocalVariableType> -> DefinedMethodBody
    new: unit -> DefinedMethodBody

    abstract WriteInstructions: byref<MethodBodyBuilder> * MetadataTokenSource -> uint16

type EntryPoint

[<RequireQualifiedAccess>]
module EntryPoint =
    //val (|None|Method|File|): EntryPoint -> Choice<_, _, _>
    val (|None|Method|): EntryPoint -> Choice<unit, struct(DefinedType * EntryPointMethod)>

[<IsReadOnly; Struct>]
[<NoComparison; NoEquality>]
type CustomAttributeList =
    member Count: int32
    member Add: CustomAttribute -> IValidationError option

[<Sealed>]
type DefinedTypeMembers =
    [<DefaultValue>] val mutable internal Field: HybridHashSet<DefinedField>
    [<DefaultValue>] val mutable internal Method: HybridHashSet<DefinedMethod>
    [<DefaultValue>] val mutable internal MethodBodyLookup: LateInitDictionary<DefinedMethod, DefinedMethodBody>

    member FieldCount: int32
    member MethodCount: int32
    //member PropertyCount: int32
    //member EventCount: int32

    member DefineMethod:
        method: DefinedMethod *
        body: DefinedMethodBody voption *
        attributes: CustomAttributeList ref voption -> ValidationResult<MethodCallTarget<DefinedType, DefinedMethod>>

    member DefineEntryPoint:
        method: EntryPointMethod *
        body: DefinedMethodBody *
        attributes: CustomAttributeList ref voption ->
            ValidationResult<MethodCallTarget<DefinedType, MethodDefinition<MethodKinds.Static>>>

    member ContainsField: field: DefinedField -> bool
    member ContainsMethod: method: DefinedMethod -> bool

//[<IsReadOnly; Struct>]
//type DefinedTypeMembers<'Owner when 'Owner :> DefinedType> =
//    val Members: DefinedTypeMembers

[<Sealed>]
type ReferencedTypeMembers =
    [<DefaultValue>] val mutable internal Field: HybridHashSet<ReferencedField>
    [<DefaultValue>] val mutable internal Method: HybridHashSet<ReferencedMethod>

    member FieldCount: int32
    member MethodCount: int32
    //member PropertyCount: int32
    //member EventCount: int32

    member ReferenceMethod: method: ReferencedMethod -> ValidationResult<MethodCallTarget<ReferencedType, ReferencedMethod>>

    member ContainsField: field: ReferencedField -> bool
    member ContainsMethod: method: ReferencedMethod -> bool

[<IsReadOnly; Struct>]
type ReferencedTypeMembers<'Kind when 'Kind :> IAttributeTag<TypeDefFlags>> =
    val Members: ReferencedTypeMembers

[<AbstractClass; Sealed; Extension>]
type TypeMemberExtensions =
    [<Extension>]
    static member ReferenceMethod :
        members: ReferencedTypeMembers<'Kind> *
        method: MethodReference<MethodKinds.ObjectConstructor> ->
            ValidationResult<MethodCallTarget<TypeReference<'Kind>, MethodReference<MethodKinds.ObjectConstructor>>>
            when 'Kind :> TypeKinds.IHasConstructor

    [<Extension>]
    static member ReferenceMethod :
        members: ReferencedTypeMembers<'Kind> *
        method: MethodReference<MethodKinds.Static> ->
            ValidationResult<MethodCallTarget<TypeReference<'Kind>, MethodReference<MethodKinds.Static>>>
            when 'Kind :> TypeAttributes.IHasStaticMethods

    //static member DefineEntryPoint

/// Builds a CLI metadata module (I.9).
[<Sealed>]
type CliModuleBuilder =
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

    // TODO: Expose constructors for types in Cli namespace.
    member DefineType: definition: DefinedType -> ValidationResult<struct(CustomAttributeList * DefinedTypeMembers)>
    member DefineType: definition: DefinedType * attributes: CustomAttributeList ref voption -> ValidationResult<DefinedTypeMembers>

    //member DefineType: DefinedType * attributes: outref<CustomAttributeList> -> ValidationResult<DefinedTypeMembers>

    // TODO: For specific TypeDefinition kinds, return a struct that wraps DefinedTypeMembers and only allows addition of certain members.
    //member DefineType: TypeDefinition<TypeKinds.StaticClass> -> ValidationResult<>

    member ReferenceType: reference: ReferencedType -> ValidationResult<ReferencedTypeMembers>
    member ReferenceType: reference: TypeReference<'Kind> -> ValidationResult<ReferencedTypeMembers<'Kind>>

    member internal Serialize: unit -> CliMetadataBuilder
