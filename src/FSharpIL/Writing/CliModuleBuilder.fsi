namespace FSharpIL.Writing

open System
open System.Collections.Generic

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Cil

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
type DefinedTypeMembers =
    [<DefaultValue>] val mutable internal Field: HybridHashSet<DefinedField>
    [<DefaultValue>] val mutable internal Method: HybridHashSet<DefinedMethod>
    [<DefaultValue>] val mutable internal MethodBodyLookup: LateInitDictionary<DefinedMethod, DefinedMethodBody>

    member FieldCount: int32
    member MethodCount: int32
    //member PropertyCount: int32
    //member EventCount: int32

    // TODO: For these add methods, also return a mutable list of custom attributes

[<Sealed>]
type ReferencedTypeMembers =
    [<DefaultValue>] val mutable internal Field: HybridHashSet<ReferencedField>
    [<DefaultValue>] val mutable internal Method: HybridHashSet<ReferencedMethod>

    internal new: owner: ReferencedType * warnings: ValidationWarningsBuilder option -> ReferencedTypeMembers

    member FieldCount: int32
    member MethodCount: int32
    //member PropertyCount: int32
    //member EventCount: int32

[<Sealed>]
type CustomAttributeList =
    member Count: int32
    member Add: CustomAttribute -> ValidationResult<unit>

// TODO: Instead of having return values of methods by ValidationResult, have the ModuleBuilder instance itself keep track of whether or not it is an "error". This means that methods that return an error do not leave the builder in a potentially weird state.

/// Builds a CLI metadata module (I.9).
[<Sealed>]
type CliModuleBuilder =
    new :
        name: Identifier *
        ?mvid: Guid *
        ?header: CliHeader *
        ?root: CliMetadataRoot<FSharpIL.Omitted, FSharpIL.Omitted> *
        ?assembly: DefinedAssembly *
        ?warnings: ValidationWarningsBuilder *
        ?typeDefCapacity: int32 *
        ?typeRefCapacity: int32 *
        ?typeSpecCapacity: int32 *
        ?assemblyRefCapacity: int32 -> CliModuleBuilder

    member Mvid: Guid
    member Name: Identifier
    member ModuleCustomAttributes: CustomAttributeList
    member Assembly: DefinedAssembly option
    member AssemblyCustomAttributes: CustomAttributeList option
    member EntryPoint: EntryPoint // TODO: Allow a File row to also be an entry point.
    member DefinedTypes: IReadOnlyCollection<DefinedType>
    member ReferencedTypes: IReadOnlyCollection<ReferencedType>
    member ReferencedAssemblies: IReadOnlyCollection<ReferencedAssembly>
    member ValidationWarnings: ValidationWarningsCollection
    member UserStrings: UserStringStreamBuilder
    /// <summary>
    /// Gets the members of the <c>&lt;Module&gt;</c> special type, which represents the global members defined in the module.
    /// </summary>
    member GlobalMembers: DefinedTypeMembers

    member ReferenceAssembly: ReferencedAssembly -> unit

    member internal Serialize: unit -> CliMetadataBuilder
