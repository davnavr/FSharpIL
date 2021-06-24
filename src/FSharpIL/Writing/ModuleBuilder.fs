namespace FSharpIL.Writing

open System
open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL.Cli
open FSharpIL.Metadata

open FSharpIL.Utilities.Collections

[<Sealed>]
type DefinedTypeMembers internal (warnings) =
    let mutable methods = HybridHashSet<DefinedMethod>()
    member _.MethodCount = methods.Count

[<Sealed>]
type ReferencedTypeMembers internal (warnings) = class end

[<Sealed>]
type TypeMemberLookup<'Type, 'Members, 'Validator
    when 'Type : equality
    and 'Type : not struct
    and 'Validator :> IMetadataValidator<'Type>
    and 'Validator : struct>
    (
        capacity: int32,
        init,
        warnings
    )
    =
    let lookup = Dictionary<'Type, 'Members> capacity

    member _.Count = lookup.Count
    member _.Add t =
        match lookup.TryGetValue t with
        | true, _ -> Error(Unchecked.defaultof<'Validator>.Duplicate t)
        | false, _ ->
            match Unchecked.defaultof<'Validator>.Validate(t, warnings) with
            | None ->
                let members = init warnings
                lookup.[t] <- members
                Ok members
            | Some err -> Error err

type DefinedTypeCollection = TypeMemberLookup<DefinedType, DefinedTypeMembers, DefinedTypeValidator>
type ReferencedTypeCollection = TypeMemberLookup<ReferencedType, ReferencedTypeMembers, ReferencedTypeValidator>

[<Sealed>]
type ModuleBuilder
    (
        mvid,
        name,
        ?assembly: AssemblyDefinition,
        ?warnings,
        ?typeDefCapacity,
        ?typeRefCapacity,
        ?assemblyRefCapacity
    ) =
    let assemblyRefs = HashSet<AssemblyReference>(defaultArg assemblyRefCapacity 8)

    member val ValidationWarnings = ValidationWarningsCollection(?warnings = warnings)
    member _.Mvid: Guid = mvid
    member _.Name: Identifier = name
    member _.Assembly = assembly
    member val DefinedTypes = DefinedTypeCollection(defaultArg typeDefCapacity 16, DefinedTypeMembers, warnings)
    member val ReferencedTypes = ReferencedTypeCollection(defaultArg typeRefCapacity 32, ReferencedTypeMembers, warnings)
    member _.ReferencedAssemblies = assemblyRefs :> IReadOnlyCollection<_>
