/// Contains functions for building Portable Executable files (II.25).
[<RequireQualifiedAccess>]
module FSharpIL.Writing.BuildPE

open System

open FSharpIL
open FSharpIL.Metadata
open FSharpIL.PortableExecutable

/// <summary>Creates a Portable Executable file with sections containing arbitrary data.</summary>
/// <param name="fileHeader">The Common Object File Format (COFF) header of the PE file.</param>
/// <param name="optionalHeader">The optional header of the PE file.</param>
/// <param name="sections">The array of functions used to generate the sections of the PE file.</param>
/// <returns>The generated Portable Executable file.</returns>
val ofSectionBuilders<'State> :
    fileHeader: CoffHeader<Omitted, Omitted> ->
    optionalHeader: OptionalHeader ->
    sections: System.Collections.Immutable.ImmutableArray<SectionBuilder<'State>> ->
    PEFile

val ofMetadataBuilder : flags: FileCharacteristics -> builder: CliMetadataBuilder -> PEFile

/// <summary>Creates a Portable Executable file from a CLI metadata module.</summary>
val ofModuleBuilder : flags: FileCharacteristics -> builder: CliModuleBuilder -> PEFile

// TODO: Consider making a version that uses a struct implementing an interface, and make sure to benchmark it.
// TODO: Make mvid an option to generate Guid deterministically based on the contents of the module.

/// <summary>Creates a Portable Executable file containing a CLI metadata module.</summary>
/// <param name="flags">Flags that specify whether the file is a library or an executable.</param>
/// <param name="header">The fields of the CLI header.</param>
/// <param name="root">
/// The fields of the CLI metadata root, which specifies what version of the Common Language Runtime that the file can run on.
/// </param>
/// <param name="name">The name of the CLI module.</param>
/// <param name="mvid">
/// An identifier used to distinguish between two versions of the same module, can be randomly generated.
/// </param>
/// <param name="builder">
/// The collection of functions used to update the contents of the CLI module and to update the state given the modifications
/// made to the CLI module or any generated validation warnings.
/// </param>
/// <param name="state">The initial state.</param>
/// <returns>
/// If successful, the portable executable file containing the CLI module and the final state value; otherwise, an error object
/// describing why the generated CLI metadata was invalid.
/// </returns>
val ofModule<'State> :
    flags: FileCharacteristics ->
    header: CliHeader ->
    root: CliMetadataRoot<Omitted, Omitted> ->
    name: Identifier ->
    mvid: Guid ->
    builder: BuildCli.ModuleBuilder<'State> ->
    state: 'State ->
    ValidationResult<PEFile * 'State>
