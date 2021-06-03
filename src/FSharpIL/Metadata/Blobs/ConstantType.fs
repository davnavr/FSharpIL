namespace FSharpIL.Metadata.Blobs

/// Specifies the type of a constant value (II.22.9).
type ConstantType =
    | Boolean = 0x2uy
    | Char = 0x3uy
    | I1 = 0x4uy
    | U1 = 0x5uy
    | I2 = 0x6uy
    | U2 = 0x7uy
    | I4 = 0x8uy
    | U4 = 0x9uy
    | I8 = 0xAuy
    | U8 = 0xBuy
    | R4 = 0xCuy
    | R8 = 0xDuy
    | String = 0xEuy
    | Null = 0x12uy

[<RequireQualifiedAccess>]
module ConstantType =
    let inline toElementType (constantType: ConstantType) = LanguagePrimitives.EnumOfValue<_, ElementType>(uint8 constantType)
