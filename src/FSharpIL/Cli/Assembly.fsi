namespace FSharpIL.Cli

open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

[<NoComparison; CustomEquality>]
type ReferencedAssembly =
    { Version: AssemblyVersion
      PublicKeyOrToken: PublicKeyOrToken
      Name: FileName
      Culture: Identifier voption
      HashValue: ImmutableArray<byte> }

    member Flags: AssemblyFlags

    override ToString: unit -> string
    override GetHashCode: unit -> int32
    override Equals: obj -> bool

    interface IEquatable<ReferencedAssembly>

[<NoComparison; CustomEquality>]
type DefinedAssembly =
    { Version: AssemblyVersion
      //Flags: AssemblyFlags
      PublicKey: ImmutableArray<byte>
      Name: FileName
      Culture: Identifier voption }

    override ToString: unit -> string
    override GetHashCode: unit -> int32
    override Equals: obj -> bool

    interface IEquatable<DefinedAssembly>
