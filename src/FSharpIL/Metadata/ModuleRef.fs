﻿namespace FSharpIL.Metadata

/// <summary>(0x1A) Represents a row in the <c>ModuleRef</c> table (II.22.31).</summary>
/// <seealso cref="T:FSharpIL.Metadata.FileTable"/>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
[<StructuralComparison; StructuralEquality>]
type ModuleRef =
    { /// <summary>
      /// The corresponding entry in the <c>File</c> table that allows "the CLI to locate the target module".
      /// </summary>
      File: RawIndex<File> }

/// <summary>Warning used when there is a duplicate row in the <c>ModuleRef</c> table (2).</summary>
/// <category>Warnings</category>
[<Sealed>]
type DuplicateModuleRefWarning (duplicate: ModuleRef) =
    inherit ValidationWarning()
    member _.Duplicate = duplicate
