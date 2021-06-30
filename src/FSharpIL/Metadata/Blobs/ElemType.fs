namespace FSharpIL.Metadata.Blobs

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type PrimitiveElemType =
    | Bool
    | Char
    | R4
    | R8
    | I1
    | I2
    | I4
    | I8
    | U1
    | U2
    | U4
    | U8
    | String
    | Type
    //| Boxed // of PrimitiveElemType

/// <summary>Represents the type of a custom attribute argument (II.23.3).</summary>
[<RequireQualifiedAccess>]
type ElemType =
    | Primitive of PrimitiveElemType
    | SZArray of PrimitiveElemType
    // TODO: How to represent enum types?
