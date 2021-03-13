namespace FSharpIL.Metadata

open System

[<NoComparison; StructuralEquality>]
type PublicKeyOrToken =
    /// Stores the full public key.
    | PublicKey of byte[]
    /// Stores the lower 8 bytes of the SHA-1 hash of the originator's public key
    | PublicKeyToken of byte * byte * byte * byte * byte * byte * byte * byte
    | NoPublicKey

/// <summary>(0x23) Represents a row in the <c>AssemblyRef</c> table (II.22.5)</summary>
[<NoComparison; CustomEquality>]
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

    override this.ToString() =
        let culture =
            match string this.Culture with
            | "" -> "neutral"
            | name -> name
        let token =
            match this.PublicKeyOrToken with
            | PublicKeyToken(b1, b2, b3, b4, b5, b6, b7, b8) ->
                sprintf ", PublicKeyToken=%02x%02x%02x%02x%02x%02x%02x%02x" b1 b2 b3 b4 b5 b6 b7 b8
            | _ -> ""
        sprintf
            "%O, Version=%O, Culture=%s%s"
            this.Name
            this.Version
            culture
            token

    interface IEquatable<AssemblyRef> with
        member this.Equals other =
            this.Version = other.Version
            && this.PublicKeyOrToken = other.PublicKeyOrToken
            && this.Name = other.Name // TODO: Make AssemblyRef name comparison case-blind.
            && this.Culture = other.Culture

/// <summary>Warning used when there is a duplicate row in the <c>AssemblyRef</c> table (10).</summary>
/// <category>Warnings</category>
[<Sealed>]
type DuplicateAssemblyRefWarning (duplicate: AssemblyRef) =
    inherit ValidationWarning()
    member _.Duplicate = duplicate
    override this.ToString() =
        sprintf
            "A duplicate reference to \"%O\" was added when a reference to an assembly with the same version, public key, name, and culture already exists"
            this.Duplicate
