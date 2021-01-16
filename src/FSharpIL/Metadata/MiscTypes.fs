namespace FSharpIL.Metadata

type AssemblyCulture = // TODO: Add more cultures
    | NullCulture
    | Ar_SA
    | En_US
    | Div_MV

type PublicKeyOrToken =
    /// Stores the full public key.
    | PublicKey of byte[]
    /// Stores the lower 8 bytes of the SHA-1 hash of the originator's public key
    | PublicKeyToken of byte * byte * byte * byte * byte * byte * byte * byte
    | NoPublicKey

type HashAlgorithmId =
    | None = 0u
    | MD5 = 0x8003u
    | SHA1 = 0x8004u
