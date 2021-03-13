namespace rec FSharpIL.Metadata

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

    override this.ToString() =
        match this with
        | AssemblyRef assembly -> string assembly.Value.Name
        | _ -> "unknown resolution scope"

/// <summary>
/// (0x01) Represents a row in the <c>TypeRef</c> table (II.22.38).
/// </summary>
[<StructuralComparison; StructuralEquality>]
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type TypeRef =
    { ResolutionScope: ResolutionScope
      TypeName: Identifier
      TypeNamespace: string }

    override this.ToString() =
        let name =
            if this.TypeNamespace.Length > 0
            then sprintf "%s.%A" this.TypeNamespace this.TypeName
            else string this.TypeName
        sprintf "[%O]%s" this.ResolutionScope name

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
