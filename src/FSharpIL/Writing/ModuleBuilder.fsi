namespace FSharpIL.Writing

open System
open System.Collections.Generic
open System.Collections.Immutable

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

    abstract WriteInstructions: byref<MethodBodyBuilder> -> uint16

type EntryPoint

[<RequireQualifiedAccess>]
module EntryPoint =
    //val (|None|Method|File|): EntryPoint -> Choice<_, _, _>
    val (|None|Method|): EntryPoint -> Choice<unit, struct(DefinedType * EntryPointMethod)>

[<Sealed>]
type DefinedTypeMembers =
    [<DefaultValue>] val mutable internal Method: HybridHashSet<DefinedMethod>
    [<DefaultValue>] val mutable internal MethodBodyLookup: LateInitDictionary<DefinedMethod, DefinedMethodBody>

    //member FieldCount: int32
    member MethodCount: int32
    //member PropertyCount: int32
    //member EventCount: int32

    member AddMethod: method: DefinedMethod * body: DefinedMethodBody voption -> ValidationResult<unit> // TODO: Have return type be an object that allows the calling of the method in a method body.
    member AddEntryPoint: method: EntryPointMethod * body: DefinedMethodBody -> ValidationResult<unit>
    member ContainsMethod: method: DefinedMethod -> bool

[<Sealed>]
type ReferencedTypeMembers =
    [<DefaultValue>] val mutable internal Method: HybridHashSet<ReferencedMethod>

    internal new: owner: ReferencedType * warnings: ValidationWarningsBuilder option -> ReferencedTypeMembers

    //member FieldCount: int32
    member MethodCount: int32
    //member PropertyCount: int32
    //member EventCount: int32

    member ReferenceMethod: ReferencedMethod -> ValidationResult<unit>
    member ContainsMethod: method: ReferencedMethod -> bool

[<Sealed>]
type CustomAttributeList =
    member Count: int32
    member Add: CustomAttribute -> ValidationResult<unit>

/// Builds a CLI metadata module (I.9).
[<Sealed>]
type ModuleBuilder =
    new :
        name: Identifier *
        ?mvid: Guid *
        ?assembly: AssemblyDefinition *
        ?warnings: ValidationWarningsBuilder *
        ?typeDefCapacity: int32 *
        ?typeRefCapacity: int32 *
        ?assemblyRefCapacity: int32 -> ModuleBuilder

    member Mvid: Guid
    member Name: Identifier
    member ModuleCustomAttributes: CustomAttributeList
    member Assembly: AssemblyDefinition option
    member AssemblyCustomAttributes: CustomAttributeList option
    member EntryPoint: EntryPoint // TODO: Allow a File row to also be an entry point.
    member DefinedTypes: IReadOnlyCollection<DefinedType>
    member ReferencedTypes: IReadOnlyCollection<ReferencedType>
    member ReferencedAssemblies: IReadOnlyCollection<AssemblyReference>
    member ValidationWarnings: ValidationWarningsCollection
    member UserStrings: UserStringStreamBuilder
    //member Globals: DefinedTypeMembers

    member DefineType: DefinedType -> ValidationResult<struct(DefinedTypeMembers * CustomAttributeList)>
    member ReferenceType: ReferencedType -> ValidationResult<ReferencedTypeMembers> // TODO: Apparently TypeRefs can have custom attributes.

    member ReferenceAssembly: AssemblyReference -> unit

    member internal Serialize: unit -> CliMetadataBuilder
