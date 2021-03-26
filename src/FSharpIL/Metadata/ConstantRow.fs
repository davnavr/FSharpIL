namespace FSharpIL.Metadata

open System.Runtime.CompilerServices

type ConstantParentTag =
    | Field = 0uy
    | Param = 1uy
    | Property = 2uy

type ConstantParent = TaggedIndex<ConstantParentTag>

type IntegerType =
    | Bool = 0uy
    | Char = 1uy
    | I1 = 2uy
    | U1 = 3uy
    | I2 = 4uy
    | U2 = 5uy
    | I4 = 6uy
    | U4 = 7uy
    | I8 = 8uy
    | U8 = 9uy

/// <summary>Represents an integer value in the <c>#Blob</c> heap that is referenced by the <c>Constant</c> table.</summary>
[<IsReadOnly; Struct>]
type IntegerConstant internal (tag: IntegerType, value: int64) =
    internal new (tag, value: uint64) = IntegerConstant(tag, int64 value)
    new (value: bool) = IntegerConstant(IntegerType.Bool, if value then 1L else 0L)
    new (value: char) = IntegerConstant(IntegerType.Char, int64 value)
    new (value: int8) = IntegerConstant(IntegerType.I1, int64 value)
    new (value: uint8) = IntegerConstant(IntegerType.U1, int64 value)
    new (value: int16) = IntegerConstant(IntegerType.I2, int64 value)
    new (value: uint16) = IntegerConstant(IntegerType.U2, int64 value)
    new (value: int32) = IntegerConstant(IntegerType.I4, int64 value)
    new (value: uint32) = IntegerConstant(IntegerType.U4, int64 value)
    new (value: int64) = IntegerConstant(IntegerType.I8, value)
    new (value: uint64) = IntegerConstant(IntegerType.U8, value)
    member _.Tag = tag
    member internal _.Type = LanguagePrimitives.EnumOfValue(uint8 tag + 2uy): ElementType
    member _.Bool = value <> 0L
    member _.Char = char value
    member _.I1 = int8 value
    member _.U1 = uint8 value
    member _.I2 = int16 value
    member _.U2 = uint16 value
    member _.I4 = int32 value
    member _.U4 = uint32 value
    member _.I8 = value
    member _.U8 = uint64 value

    override this.ToString() =
        match tag with
        | IntegerType.I4 -> sprintf "int32(%i)" this.I4
        | _ -> "?"

[<RequireQualifiedAccess>]
[<IsReadOnly; Struct>]
type ConstantValue =
    | Null
    | Integer of int: IntegerConstant
    //| Real of flt: RealConstant
    | String of str: string

/// <summary>Represents a row in the <c>Constant</c> table (II.22.9).</summary>
[<IsReadOnly; Struct>]
type ConstantRow internal (parent: ConstantParent, value: ConstantValue) =
    member _.Parent = parent
    member _.Value = value
