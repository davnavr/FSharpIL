/// Contains functions for obtaining the bytes of various integer types.
[<RequireQualifiedAccess>]
module FSharpIL.Utilities.BytesOf

let (|U2|) (value: uint16) =
    struct (
        (value >>> 8) &&& 0xFFus |> byte, // MSB
        value &&& 0xFFus |> byte // LSB
    )

let (|U4|) (value: uint32) =
    struct (
        (value >>> 24) &&& 0xFFu |> byte, // MSB
        (value >>> 16) &&& 0xFFu |> byte,
        (value >>> 8) &&& 0xFFu |> byte,
        value &&& 0xFFu |> byte // LSB
    )

let (|U8|) (value: uint64) =
    struct (
        (value >>> 56) &&& 0xFFUL |> byte, // MSB
        (value >>> 48) &&& 0xFFUL |> byte,
        (value >>> 40) &&& 0xFFUL |> byte,
        (value >>> 32) &&& 0xFFUL |> byte,
        (value >>> 24) &&& 0xFFUL |> byte,
        (value >>> 16) &&& 0xFFUL |> byte,
        (value >>> 8) &&& 0xFFUL |> byte,
        value &&& 0xFFUL |> byte // LSB
    )
