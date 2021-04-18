namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL

type ConstantParentTag =
    | Field = 0uy
    | Param = 1uy
    | Property = 2uy

type ConstantParent = TaggedIndex<ConstantParentTag>

type ConstantValueType =
    | Null = 0uy
    // Rest of values follow element types (II.23.1.16)
    | Boolean = 2uy
    | Char = 3uy
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

[<AutoOpen>]
module ConstantTypeExtensions =
    type ConstantValueType with
        member internal this.IntegerType: IntegerType = LanguagePrimitives.EnumOfValue(uint8 this - 2uy)
    type IntegerType with
        member internal this.ElementType: ElementType = LanguagePrimitives.EnumOfValue(uint8 this + 2uy)
        member this.ConstantType: ConstantValueType = LanguagePrimitives.EnumOfValue(this.ElementType |> uint8)

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
        | IntegerType.I8 -> sprintf "int64(%i)" this.I4
        | _ -> sprintf "0x%8x" this.U8

[<IsReadOnly>]
type IntegerConstantBlob = struct
    val Tag: IntegerType
    val internal Index: int32
    internal new (tag, index) = { Tag = tag; Index = index }
end

[<IsReadOnly; Struct>]
[<StructuralComparison; StructuralEquality>]
type ConstantString (str: string) = override _.ToString() = str

[<AutoOpen>]
module ConstantString =
    let (|ConstantString|) (str: ConstantString) = str.ToString()

[<IsReadOnly>]
type ConstantBlob = struct
    val Tag: ConstantValueType
    val internal Index: int32 // NOTE: This index shouldn't be turned into a Blob<_>, and instead refers to an index into one of ConstantBlobLookup's arrays.
    internal new (tag, index) = { Tag = tag; Index = index }
    new (str: Blob<ConstantString>) = ConstantBlob(ConstantValueType.String, str.Index)
    new (int: IntegerConstantBlob) = ConstantBlob(int.Tag.ConstantType, int.Index)
end

[<RequireQualifiedAccess>]
module ConstantBlob =
    // |Real|
    let (|Null|Integer|String|) (constant: ConstantBlob) =
        match constant.Tag with
        | ConstantValueType.Null -> Null
        | ConstantValueType.String -> String(Blob<ConstantString> constant.Index)
        | ConstantValueType.Boolean
        | ConstantValueType.Char
        | ConstantValueType.I1
        | ConstantValueType.U1
        | ConstantValueType.I2
        | ConstantValueType.U2
        | ConstantValueType.I4
        | ConstantValueType.U4
        | ConstantValueType.I8
        | ConstantValueType.U8 -> Integer(IntegerConstantBlob(constant.Tag.IntegerType, constant.Index))
        | _ -> invalidArg "constant" "Invalid constant value kind"
    let Null = ConstantBlob(ConstantValueType.Null, 0)
    let Integer (int: IntegerConstantBlob) = ConstantBlob int
    let String (str: Blob<ConstantString>) = ConstantBlob str
    //let Float (flt: FloatConstantBlob)

/// <summary>Represents a row in the <c>Constant</c> table (II.22.9).</summary>
[<IsReadOnly; Struct>]
type ConstantRow internal (parent: ConstantParent, value: ConstantBlob) =
    member _.Parent = parent
    member _.Value = value

[<Sealed>]
type ConstantBlobLookup internal
    (
        integers: ImmutableArray<IntegerConstant>
        //floats: ImmutableArray<FloatConstant>,
        //strings: ImmutableArray<ConstantString>
    ) =
    member _.Count = integers.Length // +
    member _.Integers = integers
    member _.Item with get (i: IntegerConstantBlob) =
        let index = i.Index
        &integers.ItemRef index

// TODO: Don't forget to include counts of floats and strings later.

[<Sealed>]
type ConstantBlobLookupBuilder internal () =
    let integers = Dictionary<IntegerConstant, int32>()
    //let floats = Dictionary<FloatConstant, int32>()
    //let strings = Dictionary<ConstantString, int32>()

    member _.TryAdd(int: IntegerConstant) =
        let tag, i = int.Tag, integers.Count
        match integers.TryGetValue int with
        | true, existing -> Error(IntegerConstantBlob(tag, existing))
        | false, _ ->
            integers.[int] <- i
            Ok(IntegerConstantBlob(tag, i))

    member this.GetOrAdd(int: IntegerConstant) = this.TryAdd int |> Result.any

    member _.Count = integers.Count // +

    member internal _.ToImmutable() =
        let mutable integers' = Array.zeroCreate<IntegerConstant> integers.Count
        for KeyValue(int, i) in integers do
            integers'.[i] <- int
        ConstantBlobLookup(Unsafe.As &integers')
