namespace FSharpIL.Metadata

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

/// Stores the lower 8 bytes of the SHA-1 hash of the originator's public key (II.6.3).
[<IsReadOnly; Struct>]
[<NoComparison; StructuralEquality>]
type PublicKeyToken (b1: uint8, b2: uint8, b3: uint8, b4: uint8, b5: uint8, b6: uint8, b7: uint8, b8: uint8) =
    member _.ToArray() = [| b1; b2; b3; b4; b5; b6; b7; b8; |]
    override _.ToString() = sprintf ".publickeytoken = (%2X %2X %2X %2X %2X %2X %2X %2X)" b1 b2 b3 b4 b5 b6 b7 b8

type internal PublicKeyOrTokenTag =
    | NoPublicKey = 0uy
    | PublicKey = 1uy
    | PublicKeyToken = 2uy

[<IsReadOnly; Struct>]
type PublicKeyOrToken internal (tag: PublicKeyOrTokenTag, index: Blob<ImmutableArray<byte>>) =
    member internal _.Tag = tag
    member internal _.Index = index
    new (publicKeyToken: Blob<PublicKeyToken>) = PublicKeyOrToken(PublicKeyOrTokenTag.PublicKeyToken, publicKeyToken.ChangeTag())
    new (publicKey: Blob<ImmutableArray<byte>>) = PublicKeyOrToken(PublicKeyOrTokenTag.PublicKey, publicKey)
    member _.AsByteBlob() =
        match tag with
        | PublicKeyOrTokenTag.NoPublicKey -> ValueNone
        | _ -> ValueSome index

[<AutoOpen>]
module PublicKeyOrToken =
    let (|PublicKey|PublicKeyToken|NoPublicKey|) (blob: PublicKeyOrToken) =
        match blob.Tag with
        | PublicKeyOrTokenTag.NoPublicKey -> NoPublicKey
        | PublicKeyOrTokenTag.PublicKey -> PublicKey blob.Index
        | PublicKeyOrTokenTag.PublicKeyToken -> PublicKeyToken(blob.Index.ChangeTag<PublicKeyToken>())
        | _ -> invalidArg "blob" "Invalid public key or token"

    let NoPublicKey = PublicKeyOrToken(PublicKeyOrTokenTag.NoPublicKey, Blob 0)

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
        sprintf
            "%O, Version=%O, Culture=%s"
            this.Name
            this.Version
            culture

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
