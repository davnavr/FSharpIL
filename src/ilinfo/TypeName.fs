[<RequireQualifiedAccess>]
module ILInfo.TypeName

open FSharpIL.Metadata

let ofTypeDefOrRefOrSpec _ wr = fprintf wr "// TODO: Get name of type"

let encoded etype wr =
    match etype with
    | EncodedType.Var i -> fprintfn wr "!%i" i
    | EncodedType.MVar i -> fprintfn wr "!!%i" i
    | EncodedType.Boolean -> wr.Write "bool"
    | EncodedType.Char -> wr.Write "char"
    | EncodedType.I1 -> wr.Write "int8"
    | EncodedType.U1 -> wr.Write "unsigned int8"
    | EncodedType.I2 -> wr.Write "int16"
    | EncodedType.U2 -> wr.Write "unsigned int16"
    | EncodedType.I4 -> wr.Write "int32"
    | EncodedType.U4 -> wr.Write "unsigned int32"
    | EncodedType.I8 -> wr.Write "int64"
    | EncodedType.U8 -> wr.Write "unsigned int64"
    | EncodedType.R4 -> wr.Write "float32"
    | EncodedType.R8 -> wr.Write "float64"
    | EncodedType.I -> wr.Write "native int"
    | EncodedType.U -> wr.Write "native unsigned int"
    | EncodedType.Object -> wr.Write "object"
    | EncodedType.String -> wr.Write "string"
    | bad -> fprintf wr "TODO: Add support for \"%A\"" bad
