[<RequireQualifiedAccess>]
module internal FSharpIL.Utilities.Bytes

open System

/// Writes the bytes of the 16-bit integer in little endian order.
let ofU2 (bytes: Span<byte>) (value: uint16) =
    bytes.[0] <- value &&& 0xFFus |> byte // LSB
    bytes.[1] <- (value >>> 8) &&& 0xFFus |> byte // MSB
    bytes

/// Writes the bytes of the 32-bit integer in little endian order.
let ofU4 (bytes: Span<byte>) (value: uint32) =
    bytes.[0] <- value &&& 0xFFu |> byte // LSB
    bytes.[1] <- (value >>> 8) &&& 0xFFu |> byte
    bytes.[2] <- (value >>> 16) &&& 0xFFu |> byte
    bytes.[3] <- (value >>> 24) &&& 0xFFu |> byte // MSB
    bytes

/// Writes the bytes of the 64-bit integer in little endian order.
let ofU8 (bytes: Span<byte>) (value: uint64) =
    bytes.[0] <- value &&& 0xFFUL |> byte // LSB
    bytes.[1] <- (value >>> 8) &&& 0xFFUL |> byte
    bytes.[2] <- (value >>> 16) &&& 0xFFUL |> byte
    bytes.[3] <- (value >>> 24) &&& 0xFFUL |> byte
    bytes.[4] <- (value >>> 32) &&& 0xFFUL |> byte
    bytes.[5] <- (value >>> 40) &&& 0xFFUL |> byte
    bytes.[6] <- (value >>> 48) &&& 0xFFUL |> byte
    bytes.[7] <- (value >>> 56) &&& 0xFFUL |> byte // MSB
    bytes
