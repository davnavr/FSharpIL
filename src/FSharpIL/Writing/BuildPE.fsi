/// Contains functions for building Portable Executable files (II.25).
[<RequireQualifiedAccess>]
module FSharpIL.Writing.BuildPE

open System.Collections.Immutable

open FSharpIL
open FSharpIL.PortableExecutable

/// Creates a Portable Executable file.
val create:
    fileHeader: CoffHeader<Omitted, Omitted> ->
    optionalHeader: OptionalHeader ->
    sections: ImmutableArray<SectionContent> ->
    PEFile

/// Creates a Portable Executable file from a CLI metadata module.
val fromModule: flags: ImageFileFlags -> metadata: ModuleBuilder -> PEFile

/// <summary>Creates an <c>.exe</c> file from a CLI metadata module.</summary>
/// <remarks>
/// This function avoids setting a flag in the PE headers that specifies that the image is a library. Note that the file
/// extension when the Portable Executable is saved to disk doesn't have to be <c>.exe</c>, .NET Core and .NET 5+ uses
/// <c>.dll</c> as the file extension instead.
/// </remarks>
val exeFromModule: metadata: ModuleBuilder -> PEFile

/// <summary>Creates a <c>.dll</c> file from a CLI metadata module.</summary>
/// <remarks>This function sets a flag in the PE headers indicating that the image is a library.</remarks>
val dllFromModule: metadata: ModuleBuilder -> PEFile
