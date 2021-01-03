namespace FSharpIL.Metadata

type AssemblyCulture = // TODO: Add more cultures
    | NullCulture
    | Ar_SA
    | En_US
    | Div_MV

type PublicKeyOrToken =
    | PublicKey // of ?
    | HashedToken // of ?
    | NoPublicKey
