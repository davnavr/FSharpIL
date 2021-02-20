namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable

/// <summary>
/// Indicates where the target <see cref="T:FSharpIL.Metadata.TypeRef"/> is defined (II.22.38).
/// </summary>
[<RequireQualifiedAccess>]
type ResolutionScope =
    /// Indicates that "the target type is defined in the current module", and produces a warning as this value should not be used.
    | Module
    /// Indicates that the target type "is defined in another module within the same assembly as this one".
    | ModuleRef of SimpleIndex<ModuleRef>
    /// Indicates that "The target type is defined in a different assembly".
    | AssemblyRef of SimpleIndex<AssemblyRef>
    | TypeRef of SimpleIndex<TypeRef>
    | Null // of SimpleIndex<ExportedTypeRow> // TODO: How to enforce that a row exists in the ExportedType table?

/// <summary>
/// (0x01) Represents a row in the <c>TypeRef</c> table (II.22.38).
/// </summary>
and [<StructuralComparison; StructuralEquality>] TypeRef =
    { ResolutionScope: ResolutionScope
      TypeName: Identifier
      TypeNamespace: string }

    interface IIndexValue with
        member this.CheckOwner actual =
            match this.ResolutionScope with
            | ResolutionScope.Module
            | ResolutionScope.Null -> ()
            | ResolutionScope.ModuleRef mref -> IndexOwner.checkIndex actual mref
            | ResolutionScope.AssemblyRef assm -> IndexOwner.ensureEqual actual assm.Owner
            | ResolutionScope.TypeRef tref -> IndexOwner.checkIndex actual tref

/// (1d)
[<Sealed>]
type TypeRefUsesModuleResolutionScope (typeRef) =
    inherit ValidationWarning()

    member _.Type = typeRef

[<Sealed>]
type TypeRefTable internal (owner: IndexOwner, warnings: ImmutableArray<ValidationWarning>.Builder) =
    let table = MutableTable<_> owner
    let search = Dictionary<string * Identifier, TypeRef> 8

    member _.Count = table.Count

    /// <summary>
    /// Searches for a type with the specified name and namespace, with a resolution scope of <see cref="T:FSharpIL.Metadata.ResolutionScope.AssemblyRef"/>.
    /// </summary>
    member internal _.FindType((ns, name) as t) =
        match search.TryGetValue(t) with
        | (true, existing) -> SimpleIndex(owner, existing) |> Some
        | (false, _) ->
            Seq.tryPick
                (function
                | { ResolutionScope = ResolutionScope.AssemblyRef _ } as t' when t'.TypeName = name && t'.TypeNamespace = ns ->
                    search.Item <- (ns, name), t'
                    SimpleIndex(owner, t') |> Some
                | _ -> None)
                table

    member _.GetIndex typeRef =
        match table.GetIndex typeRef with
        | ValueSome index ->
            search.Item <- (typeRef.TypeNamespace, typeRef.TypeName), typeRef

            // TODO: Check that the name is a "valid CLS identifier".

            match typeRef.ResolutionScope with
            | ResolutionScope.Module ->
                TypeRefUsesModuleResolutionScope typeRef |> warnings.Add
            | _ -> ()

            Ok index
        | ValueNone -> DuplicateRowError typeRef :> ValidationError |> Error

    member _.GetEnumerator() = table.GetEnumerator()

    interface IReadOnlyCollection<TypeRef> with
        member this.Count = this.Count
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<_>
        member this.GetEnumerator() = this.GetEnumerator() :> System.Collections.IEnumerator
