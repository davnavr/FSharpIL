namespace rec FSharpIL.Cli

open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<StructuralComparison; StructuralEquality>]
type ReferencedAssembly = // TODO: Maybe rename these types to ReferencedAssembly and DefinedAssembly?
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

    override this.ToString() = this.Name.ToString()

[<StructuralComparison; StructuralEquality>]
type DefinedAssembly =
    { Version: AssemblyVersion
      //Flags: AssemblyFlags
      PublicKey: ImmutableArray<byte>
      Name: FileName
      Culture: Identifier voption }
