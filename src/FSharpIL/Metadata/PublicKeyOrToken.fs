namespace FSharpIL.Metadata

open System.Collections.Immutable

type PublicKeyOrToken =
    | NoPublicKeyOrToken
    | PublicKey of ImmutableArray<byte>
    | PublicKeyToken of byte * byte * byte * byte * byte * byte * byte * byte

[<RequireQualifiedAccess>]
module PublicKeyOrToken =
    let toBlock publicKeyOrToken =
        match publicKeyOrToken with
        | NoPublicKeyOrToken -> ImmutableArray.Empty
        | PublicKey publicKey -> publicKey
        | PublicKeyToken(b0, b1, b2, b3, b4, b5, b6, b7) ->
            let mutable token = [| b0; b1; b2; b3; b4; b5; b6; b7 |]
            System.Runtime.CompilerServices.Unsafe.As &token
