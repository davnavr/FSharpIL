namespace FSharpIL.Writing

open System
open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Cil
open FSharpIL.Metadata.Tables

open FSharpIL.Writing.Cil

open FSharpIL.Utilities.Collections

[<AbstractClass>]
type DefinedMethodBody = // TODO: Maybe move MethodBodyBuilder higher up to allow its usage in the FSharpIL.Cli namespace.
    val InitLocals: InitLocals
    val LocalTypes: Signatures.LocalVarSig

    new: localTypes: Signatures.LocalVarSig * initLocals: InitLocals -> DefinedMethodBody
    new: localTypes: Signatures.LocalVarSig -> DefinedMethodBody
    new: unit -> DefinedMethodBody

    abstract WriteInstructions: byref<MethodBodyBuilder> * MethodTokenSource * FieldTokenSource * TypeTokenSource -> uint16

type EntryPoint

[<RequireQualifiedAccess>]
module EntryPoint =
    //val (|None|Method|File|): EntryPoint -> Choice<_, _, _>
    val (|None|Method|): EntryPoint -> Choice<unit, struct(DefinedType * EntryPointMethod)>

[<Sealed>]
type CustomAttributeList =
    member Count: int32
    member Add: CustomAttribute -> ValidationResult<unit>

[<Sealed>]
type DefinedTypeMembers =
    [<DefaultValue>] val mutable internal Field: HybridHashSet<DefinedField>
    [<DefaultValue>] val mutable internal Method: HybridHashSet<DefinedMethod>
    [<DefaultValue>] val mutable internal MethodBodyLookup: LateInitDictionary<DefinedMethod, DefinedMethodBody>

    member FieldCount: int32
    member MethodCount: int32
    //member PropertyCount: int32
    //member EventCount: int32

    // TODO: For these add methods, also return a mutable list of custom attributes
    member DefineMethod:
        implFlags: MethodImplFlags *
        flags: MethodDefFlags *
        methodThis: FSharpIL.Metadata.Signatures.MethodThis *
        returnType: MethodReturnType *
        name: MethodName *
        parameterTypes: ImmutableArray<MethodParameterType> *
        parameterList: ParameterList *
        body: DefinedMethodBody voption *
        attributes: CustomAttributeList ref voption -> ValidationResult<MethodCallTarget>

    member ContainsField: field: DefinedField -> bool
    member ContainsMethod: method: DefinedMethod -> bool

[<Sealed>]
type ReferencedTypeMembers =
    [<DefaultValue>] val mutable internal Field: HybridHashSet<ReferencedField>
    [<DefaultValue>] val mutable internal Method: HybridHashSet<ReferencedMethod>

    internal new: owner: ReferencedType * warnings: ValidationWarningsBuilder option -> ReferencedTypeMembers

    member FieldCount: int32
    member MethodCount: int32
    //member PropertyCount: int32
    //member EventCount: int32

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
    member UserStrings: UserStringStreamBuilder
    /// <summary>
    /// Gets the members of the <c>&lt;Module&gt;</c> special type, which represents the global members defined in the module.
    /// </summary>
    member GlobalMembers: DefinedTypeMembers

    member DefineAssembly: assembly: DefinedAssembly -> ValidationResult<CustomAttributeList>

    /// Adds a reference to another assembly.
    member ReferenceAssembly: assembly: ReferencedAssembly -> unit

    // TODO: For methods that add things that can also have custom attributes, figure out how to avoid allocating a CustomAttributeList if user doesn't want/need the CA list.

    // TODO: Expose constructors for types in Cli namespace.

    member DefineType:
        flags: TypeDefFlags *
        extends: ClassExtends *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        genericParameters: GenericParamList -> ValidationResult<struct(CustomAttributeList * DefinedTypeMembers)>

    member DefineType:
        flags: TypeDefFlags *
        extends: ClassExtends *
        typeNamespace: Identifier voption *
        enclosingClass: DefinedType voption *
        typeName: Identifier *
        genericParameters: GenericParamList *
        attributes: CustomAttributeList ref voption -> ValidationResult<DefinedTypeMembers>

    //member DefineType: DefinedType * attributes: outref<CustomAttributeList> -> ValidationResult<DefinedTypeMembers>

    // TODO: For specific TypeDefinition kinds, return a struct that wraps DefinedTypeMembers and only allows addition of certain members.
    //member DefineType: TypeDefinition<TypeKinds.StaticClass> -> ValidationResult<>

    member internal Serialize: unit -> CliMetadataBuilder
