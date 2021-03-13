namespace FSharpIL.Metadata

open System.Collections.Generic

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
    member this.Name = this.File.Value.FileName // TODO: Make AssemblyRef name comparison case-blind.

    interface IIndexValue with
        member this.CheckOwner actual = IndexOwner.ensureEqual actual this.File.Owner

[<Sealed>]
type ModuleRefTable internal (owner: IndexOwner) =
    let modules = HashSet<ModuleRef>()

    member _.Count = modules.Count

    member _.Add moduleRef =
        IndexOwner.checkOwner owner moduleRef
        modules.Add moduleRef |> ignore
        SimpleIndex(owner, moduleRef)

    interface IReadOnlyCollection<ModuleRef> with
        member _.Count = modules.Count
        member _.GetEnumerator() = modules.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = modules.GetEnumerator() :> System.Collections.IEnumerator
