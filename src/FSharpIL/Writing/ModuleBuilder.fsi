namespace FSharpIL.Writing

open System
open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL.Cli
open FSharpIL.Metadata

open FSharpIL.Utilities.Collections

[<Sealed>]
type DefinedTypeMembers =
    internal new: owner: DefinedType * warnings: ValidationWarningsBuilder option -> DefinedTypeMembers
    member FieldCount: int32
    member MethodCount: int32
    member PropertyCount: int32
    member EventCount: int32

    member AddMethod: DefinedMethod * ValidationResult<CallTarget>

[<Sealed>]
type ReferencedTypeMembers =
    internal new: owner: ReferencedType * warnings: ValidationWarningsBuilder option -> DefinedTypeMembers
    member FieldCount: int32
    member MethodCount: int32
    member PropertyCount: int32
    member EventCount: int32

/// Builds a CLI metadata module (I.9).
[<Sealed>]
type ModuleBuilder =
    new :
        mvid: Guid *
        name: Identifier *
        ?assembly: AssemblyDefinition *
        ?warnings: ValidationWarningsBuilder *
        ?typeDefCapacity: int32 *
        ?typeRefCapacity: int32 *
        ?assemblyRefCapacity: int32 -> ModuleBuilder

[<Sealed>]
type TypeMemberLookup<'Type, 'Members, 'Validator
    when 'Type : equality
    and 'Type : not struct
    and 'Validator :> IMetadataValidator<'Type>
    and 'Validator : struct>
    =
    internal new :
        capacity: int32 *
        init: ValidationWarningsBuilder option -> 'Members *
        warnings: ValidationWarningsBuilder option -> TypeMemberLookup<'Type, 'Members, 'Validator>

    member Count: int32
    member Add: 'Type -> ValidationResult<'Members>

    interface IReadOnlyDictionary<'Type, 'Members>

type DefinedTypeCollection = TypeMemberLookup<DefinedType, DefinedTypeMembers, DefinedTypeValidator>
type ReferencedTypeCollection = TypeMemberLookup<ReferencedType, ReferencedTypeMembers, ReferencedTypeValidator>

type ModuleBuilder with
    member Mvid: Guid
    member Name: Identifier
    member Assembly: AssemblyDefinition option
    member DefinedTypes: DefinedTypeCollection
    member ReferencedTypes: ReferencedTypeCollection
    member ReferencedAssemblies: IReadOnlyCollection<AssemblyReference>
    member ValidationWarnings: IReadOnlyCollection<IValidationWarning>
