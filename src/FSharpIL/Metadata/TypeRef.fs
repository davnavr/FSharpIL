namespace rec FSharpIL.Metadata

open System.Runtime.CompilerServices

type ResolutionScopeTag =
    | Module = 0uy
    | ModuleRef = 1uy
    | AssemblyRef = 2uy
    | TypeRef = 3uy
    | Null = 4uy

/// <summary>
/// Indicates where the target <see cref="T:FSharpIL.Metadata.TypeRef"/> is defined (II.22.38).
/// </summary>
type ResolutionScope = TaggedIndex<ResolutionScopeTag>

[<RequireQualifiedAccess>]
module ResolutionScope =
    let (|Module|ModuleRef|AssemblyRef|TypeRef|Null|) (scope: ResolutionScope) =
        match scope.Tag with
        | ResolutionScopeTag.ModuleRef -> ModuleRef(scope.ToRawIndex<ModuleRef>())
        | ResolutionScopeTag.AssemblyRef -> AssemblyRef(scope.ToRawIndex<AssemblyRef>())
        | ResolutionScopeTag.TypeRef -> TypeRef(scope.ToRawIndex<TypeRef>())
        | ResolutionScopeTag.Null -> Null
        | ResolutionScopeTag.Module
        | _ -> Module

    /// Indicates that "the target type is defined in the current module", and produces a warning as this value should not be used.
    let Module = TaggedIndex ResolutionScopeTag.Module
    /// Indicates that the target type "is defined in another module within the same assembly as this one".
    let ModuleRef (index: RawIndex<ModuleRef>) = index.ToTaggedIndex ResolutionScopeTag.ModuleRef
    /// Indicates that "The target type is defined in a different assembly".
    let AssemblyRef (index: RawIndex<AssemblyRef>) = index.ToTaggedIndex ResolutionScopeTag.AssemblyRef
    let TypeRef (index: RawIndex<TypeRef>) = index.ToTaggedIndex ResolutionScopeTag.TypeRef
    let Null = TaggedIndex ResolutionScopeTag.Null // of SimpleIndex<ExportedTypeRow> // TODO: How to enforce that a row exists in the ExportedType table?

/// <summary>
/// (0x01) Represents a row in the <c>TypeRef</c> table (II.22.38).
/// </summary>
[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type TypeRef =
    { ResolutionScope: ResolutionScope
      TypeName: Identifier
      TypeNamespace: string }

/// (1d)
[<Sealed>]
type TypeRefUsesModuleResolutionScope (typeRef) =
    inherit ValidationWarning()
    member _.Type = typeRef

/// <summary>Error used when there is a duplicate row in the <c>TypeRef</c> table (6).</summary>
/// <category>Errors</category>
[<Sealed>]
type DuplicateTypeRefError (duplicate: TypeRef) =
    inherit ValidationError()
    member _.Duplicate = duplicate
    override this.ToString() =
        sprintf
            "Cannot add duplicate reference to \"%O\", a type with the same resolution scope, name, and namespace already exists"
            this.Duplicate

// TODO: Create dictionary type to handle lookup of TypeRef and TypeDef rows by name.
