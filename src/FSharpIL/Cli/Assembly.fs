namespace FSharpIL.Cli

open System
open System.Collections.Immutable

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

open FSharpIL.Utilities.Compare

[<NoComparison; CustomEquality>]
type ReferencedAssembly =
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

    override this.GetHashCode() = HashCode.Combine(this.Name, this.Version)

    interface IEquatable<ReferencedAssembly> with
        member this.Equals other = this.Version === other.Version && this.Name === other.Name

    override this.Equals obj =
        match obj with
        | :? ReferencedAssembly as other -> this === other
        | _ -> false

[<NoComparison; CustomEquality>]
type DefinedAssembly =
    { Version: AssemblyVersion
      //Flags: AssemblyFlags
      PublicKey: ImmutableArray<byte>
      Name: FileName
      Culture: Identifier voption }

    override this.ToString() = this.Name.ToString()

    override this.GetHashCode() = HashCode.Combine(this.Name, this.Version)

    interface IEquatable<DefinedAssembly> with
        member this.Equals other = this.Version === other.Version && this.Name === other.Name

    override this.Equals obj =
        match obj with
        | :? DefinedAssembly as other -> this === other
        | _ -> false
