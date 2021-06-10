[<RequireQualifiedAccess>]
module FSharpIL.Writing.BlobWriter

open FSharpIL.Utilities

open FSharpIL

let [<Literal>] MaxCompressedUnsigned = 0x1FFF_FFFFu

let checkCompressedUnsigned value =
    if value > MaxCompressedUnsigned then
        sprintf
            "Unable to compress integer %x, the maximum value for compressed unsigned integers is %x."
            value
            MaxCompressedUnsigned
        |> argOutOfRange "value" value

let compressedUnsignedSize value =
    checkCompressedUnsigned value
    if value > 0x3FFFu then 4u
    elif value > 0x7Fu then 2u
    else 1u

let compressedUnsigned value (wr: byref<ChunkedMemoryBuilder>) =
    checkCompressedUnsigned value
    if value > 0x3FFFu then
        // Sets bit 31 and bit 30, bit 29 remains clear
        wr.WriteLE(value ||| 0xC000_0000u)
    elif value > 0x7Fu then
        // Sets bit 15, bit 14 remains clear
        wr.WriteLE(uint16(value ||| 0x8000u))
    else wr.Write(uint8 value) // Bit 7 remains clear
