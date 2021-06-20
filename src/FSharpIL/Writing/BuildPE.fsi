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
