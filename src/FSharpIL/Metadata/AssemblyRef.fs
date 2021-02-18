namespace FSharpIL.Metadata

open System
open System.Collections.Generic

type PublicKeyOrToken =
    /// Stores the full public key.
    | PublicKey of byte[]
    /// Stores the lower 8 bytes of the SHA-1 hash of the originator's public key
    | PublicKeyToken of byte * byte * byte * byte * byte * byte * byte * byte
    | NoPublicKey

/// <summary>(0x23) Represents a row in the <c>AssemblyRef</c> table (II.22.5)</summary>
[<CustomEquality; NoComparison>]
type AssemblyRef =
    { Version: Version
      PublicKeyOrToken: PublicKeyOrToken
      Name: AssemblyName
      Culture: AssemblyCulture
      HashValue: unit option }

    member this.Flags =
        match this.PublicKeyOrToken with
        | PublicKey _ -> 1u
        | _ -> 0u

    interface IEquatable<AssemblyRef> with
        member this.Equals other =
            this.Version = other.Version
            && this.PublicKeyOrToken = other.PublicKeyOrToken
            && this.Name = other.Name
            && this.Culture = other.Culture

// TODO: Create new class as this shares code with ModuleRefTable
[<Sealed>]
type AssemblyRefTable internal (owner: IndexOwner) =
    let set = HashSet<AssemblyRef>()

    member _.Count = set.Count

    member _.GetIndex assemblyRef =
        if set.Add assemblyRef
        then SimpleIndex(owner, assemblyRef) |> ValueSome
        else ValueNone

    interface IReadOnlyCollection<AssemblyRef> with
        member _.Count = set.Count
        member _.GetEnumerator() = set.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = set.GetEnumerator() :> System.Collections.IEnumerator
