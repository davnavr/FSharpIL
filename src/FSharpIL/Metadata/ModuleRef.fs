namespace FSharpIL.Metadata

/// <summary>(0x1A) Represents a row in the <c>ModuleRef</c> table (II.22.31).</summary>
/// <seealso cref="T:FSharpIL.Metadata.FileTable"/>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
[<StructuralComparison; StructuralEquality>]
type ModuleRef =
    { /// <summary>
      /// The corresponding entry in the <c>File</c> table that allows "the CLI to locate the target module".
      /// </summary>
      File: SimpleIndex<File> }

    /// <summary>
    /// Corresponds to the <c>Name</c> column, which matches an entry in the <c>File</c> table.
    /// </summary>
    member this.Name = this.File.Value.FileName // TODO: Make ModuleRef name comparison case-blind.

    interface IIndexValue with
        member this.CheckOwner actual = IndexOwner.ensureEqual actual this.File.Owner

/// <summary>Warning used when there is a duplicate row in the <c>ModuleRef</c> table (2).</summary>
/// <category>Warnings</category>
[<Sealed>]
type DuplicateModuleRefWarning (duplicate: ModuleRef) =
    inherit ValidationWarning()
    member _.Duplicate = duplicate
