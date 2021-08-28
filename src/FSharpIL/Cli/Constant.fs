namespace FSharpIL.Cli

open System
open System.Runtime.CompilerServices

open FSharpIL.Utilities

[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type IntegerConstantKind =
    | Bool
    | Char
    | I1
    | U1
    | I2
    | U2
    | I4
    | U4
    | I8
    | U8

// TODO: Make this a normal union type.
[<IsReadOnly>]
[<StructuralComparison; StructuralEquality>]
type IntegerConstant = struct
    val Tag: IntegerConstantKind
    val Value: uint64

    new (value: bool) = { Tag = IntegerConstantKind.Bool; Value = if value then 1UL else 0UL }
    new (value: char) = { Tag = IntegerConstantKind.Char; Value = uint64 value }
    new (value: int8) = { Tag = IntegerConstantKind.I1; Value = uint64 value }
    new (value: uint8) = { Tag = IntegerConstantKind.U1; Value = uint64 value }
    new (value: int16) = { Tag = IntegerConstantKind.I2; Value = uint64 value }
    new (value: uint16) = { Tag = IntegerConstantKind.U2; Value = uint64 value }
    new (value: int32) = { Tag = IntegerConstantKind.I4; Value = uint64 value }
    new (value: uint32) = { Tag = IntegerConstantKind.U4; Value = uint64 value }
    new (value: int64) = { Tag = IntegerConstantKind.I8; Value = uint64 value }
    new (value: uint64) = { Tag = IntegerConstantKind.U8; Value = value }

    override this.ToString() =
        match this.Tag with
        | IntegerConstantKind.Bool -> if this.Value <> 0UL then "bool(true)" else "bool(false)"
        | IntegerConstantKind.Char -> sprintf "char(%i)" (uint16 this.Value)
        | IntegerConstantKind.I1 -> sprintf "int8(%i)" (int8 this.Value)
        | IntegerConstantKind.U1 -> sprintf "uint8(%i)" (uint8 this.Value)
        | IntegerConstantKind.I2 -> sprintf "int16(%i)" (int16 this.Value)
        | IntegerConstantKind.U2 -> sprintf "uint16(%i)" (uint16 this.Value)
        | IntegerConstantKind.I4 -> sprintf "int32(%i)" (int32 this.Value)
        | IntegerConstantKind.U4 -> sprintf "uint32(%i)" (uint32 this.Value)
        | IntegerConstantKind.I8 -> sprintf "int64(%i)" (int64 this.Value)
        | IntegerConstantKind.U8 -> sprintf "uint64(%i)" this.Value
end

[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type FloatConstantKind =
    | Single
    | Double

[<IsReadOnly; Struct>]
type FloatConstant =
    val Tag: FloatConstantKind
    val Value: uint64

    new (value: Single) = { Tag = FloatConstantKind.Single; Value = Convert.unsafeTo value }
    new (value: Double) = { Tag = FloatConstantKind.Double; Value = Convert.unsafeTo value }

[<StructuralComparison; StructuralEquality>]
type Constant =
    | NullConstant
    | StringConstant of string
    | IntegerConstant of IntegerConstant
    | FloatConstant of FloatConstant
