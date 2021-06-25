namespace rec FSharpIL.Cli

open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<System.Runtime.CompilerServices.IsReadOnly>]
type AssemblyVersion = struct
    val Major: uint16
    val Minor: uint16
    val Build: uint16
    val Revision: uint16

    new : major: uint16 * minor: uint16 * build: uint16 * revision: uint16 -> AssemblyVersion
end

[<StructuralComparison; StructuralEquality>]
type AssemblyReference =
    { Version: AssemblyVersion
      PublicKeyOrToken: PublicKeyOrToken
      Name: FileName
      Culture: Identifier voption
      HashValue: ImmutableArray<byte> }

    member Flags: AssemblyFlags

[<StructuralComparison; StructuralEquality>]
type AssemblyDefinition =
    { Version: AssemblyVersion
      //Flags: AssemblyFlags
      PublicKey: ImmutableArray<byte>
      Name: FileName
      Culture: Identifier voption }
