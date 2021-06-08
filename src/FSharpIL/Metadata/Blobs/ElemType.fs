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

[<RequireQualifiedAccess>]
type ElemType =
    | Type of PrimitiveElemType
    | SZArray of PrimitiveElemType
    | Boxed of PrimitiveElemType

//module ElemType
