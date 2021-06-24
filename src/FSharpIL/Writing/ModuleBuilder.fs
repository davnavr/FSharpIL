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
type DefinedTypeCollection internal (capacity: int32, warnings) =
    let lookup = Dictionary<DefinedType, DefinedTypeMembers> capacity

    member _.Count = lookup.Count

    member private _.Validate t =
        None

    member private this.AddUnsafe t =
        validated {
            do! this.Validate t
            let members = DefinedTypeMembers warnings
            lookup.[t] <- members
            return members
        }

    member internal this.GetOrAdd t =
        match lookup.TryGetValue t with
        | true, existing -> Ok existing
        | false, _ -> this.AddUnsafe t

    member this.Add t =
        match lookup.TryGetValue t with
        | true, _ ->
            failwith "TODO: Error case for duplicate typedef"
        | false, _ ->
            validated {
                let! members = this.GetOrAdd t
                let! _ = failwith "How to check that extends is in the ModuleBuilder, since it uses TypeDefs and Refs"
                return members
            }

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
