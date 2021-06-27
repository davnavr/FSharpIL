namespace FSharpIL.Writing

open System
open System.Collections.Generic

open FSharpIL.Cli
open FSharpIL.Metadata

open FSharpIL.Utilities.Collections

[<Sealed>]
type DefinedTypeMembers =
    [<DefaultValue>] val mutable internal Method: HybridHashSet<DefinedMethod>

    internal new: owner: DefinedType * warnings: ValidationWarningsBuilder option -> DefinedTypeMembers

    //member FieldCount: int32
    member MethodCount: int32
    //member PropertyCount: int32
    //member EventCount: int32

    member AddMethod: DefinedMethod -> ValidationResult<unit> // TODO: Have return type be an object that allows the calling of the method in a method body.

[<Sealed>]
type ReferencedTypeMembers =
    [<DefaultValue>] val mutable internal Method: HybridHashSet<ReferencedMethod>

    internal new: owner: ReferencedType * warnings: ValidationWarningsBuilder option -> ReferencedTypeMembers

    //member FieldCount: int32
    member MethodCount: int32
    //member PropertyCount: int32
    //member EventCount: int32

    //member AddMethod: ReferencedMethod -> ValidationResult<unit>

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
    member Assembly: AssemblyDefinition option
    member DefinedTypes: IReadOnlyCollection<DefinedType>
    member ReferencedTypes: IReadOnlyCollection<ReferencedType>
    member ReferencedAssemblies: IReadOnlyCollection<AssemblyReference>
    member ValidationWarnings: ValidationWarningsCollection
    //member Globals: DefinedTypeMembers

    member DefineType: DefinedType -> ValidationResult<DefinedTypeMembers>
    member ReferenceType: ReferencedType -> ValidationResult<ReferencedTypeMembers>

    member ReferenceAssembly: AssemblyReference -> unit

    member internal Serialize: unit -> CliMetadataBuilder
