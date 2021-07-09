﻿namespace rec FSharpIL.Cli

open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<System.Runtime.CompilerServices.IsReadOnly>]
type AssemblyVersion = struct // TODO: Move this type to FSharpIL.Metadata.Tables
    val Major: uint16
    val Minor: uint16
    val Build: uint16
    val Revision: uint16

    new(major, minor, build, revision) = { Major = major; Minor = minor; Build = build; Revision = revision }
end

[<StructuralComparison; StructuralEquality>]
type AssemblyReference = // TODO: Maybe rename these types to ReferencedAssembly and DefinedAssembly?
    { Version: AssemblyVersion
      PublicKeyOrToken: PublicKeyOrToken
      Name: FileName
      Culture: Identifier voption
      HashValue: ImmutableArray<byte> }

    member this.Flags =
        match this.PublicKeyOrToken with
        | NoPublicKeyOrToken
        | PublicKeyToken _ -> AssemblyFlags.None
        | PublicKey _ -> AssemblyFlags.PublicKey

[<StructuralComparison; StructuralEquality>]
type AssemblyDefinition =
    { Version: AssemblyVersion
      //Flags: AssemblyFlags
      PublicKey: ImmutableArray<byte>
      Name: FileName
      Culture: Identifier voption }
