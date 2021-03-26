[<RequireQualifiedAccess>]
module internal rec FSharpIL.Metadata.Heaps.BlobSize

[<Literal>]
let MaxCompressedUnsigned = 0x1FFF_FFFFu

/// <summary>Calculates how many bytes are needed to store the specified value in an unsigned compressed integer (II.23.2). </summary>
let ofUnsigned (value: uint32) =
    if value > MaxCompressedUnsigned then
        let msg =
            sprintf
                "Unable to compress integer %x, the maximum value for compressed unsigned integers is %x."
                value
                MaxCompressedUnsigned
        System.ArgumentOutOfRangeException("value", value, msg) |> raise
    elif value > 0x3FFFu then 4u
    elif value > 0x7Fu then 2u
    else 1u

let (|B4|B2|B1|) (value: uint32) =
    match ofUnsigned value with
    | 4u -> B4
    | 2u -> B2
    | 1u -> B1
    | _ -> invalidArg (nameof value) "Invalid compressed size."
