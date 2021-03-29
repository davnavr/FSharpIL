namespace FSharpIL.Metadata.Heaps

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Bytes
open FSharpIL.Metadata

[<IsByRefLike; IsReadOnly>]
type internal BlobWriter = struct
    val Metadata: CliMetadata
    val Writer: FSharpIL.Writing.ChunkWriter

    new (metadata, writer) = { Metadata = metadata; Writer = writer }

    member this.Position = this.Writer.Position

    /// <summary>Writes an unsigned compressed integer in big-endian order (II.23.2).</summary>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="value"/> is greater than the maximum compressed unsigned integer.
    /// </exception>
    member this.CompressedUnsigned(value: uint32) =
        match BlobSize.ofUnsigned value with
        | BlobSize.B4 ->
            // Sets bit 31 and bit 30, bit 29 remains clear
            let (U4 (msb, b2, b3, lsb)) = value ||| 0xC000_0000u
            this.Writer.WriteU1 msb
            this.Writer.WriteU1 b2
            this.Writer.WriteU1 b3
            this.Writer.WriteU1 lsb
        | BlobSize.B2 ->
            // Sets bit 15, bit 14 remains clear
            let (U2 (msb, lsb)) = uint16 (value ||| 0x8000u)
            this.Writer.WriteU1 msb
            this.Writer.WriteU1 lsb
        | BlobSize.B1 -> this.Writer.WriteU1 value // Bit 7 remains clear

    member inline this.CompressedUnsigned value = this.CompressedUnsigned(uint32 value)

    /// <summary>Writes a signed compressed integer (II.23.2).</summary>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="value"/> is not in the range <c>-2^28</c> and <c>2^28 - 1</c> inclusive.
    /// </exception>
    member this.CompressedSigned(value: int32) =
        // See https://docs.microsoft.com/en-us/dotnet/api/system.reflection.metadata.blobbuilder.writecompressedsignedinteger?view=net-5.0
        if value >= -64 && value < 64 then
            // Bit 0 contains sign bit, bit 7 remains clear
            let mutable result = abs value |> uint8
            result <- (result &&& 0b0011_1111uy) <<< 1
            if value < 0 then result <- result ||| 1uy
            this.Writer.WriteU1 value
        elif value >= -8192 && value < 8192 then // between -2^13 and 2^13 - 1 inclusive
            // Sets bit 15, bit 14 remains clear, and sign bit is stored in bit 0
            let mutable result = 32768us ||| (abs value |> uint16)
            result <- (result &&& 0b0001_1111_1111_1111us) <<< 1
            if value < 0 then result <- result ||| 1us
            let (U2 (msb, lsb)) = result
            this.Writer.WriteU1 msb
            this.Writer.WriteU1 lsb
        elif value >= -268435456 && value < 268435456 then // between -2^28 and 2^28 - 1 inclusive
            invalidOp "TODO: Implement writing of big compressed integers"
        else
            ArgumentOutOfRangeException("value", value, "Cannot compress signed integer") |> raise

    member inline this.CompressedSigned value = this.CompressedSigned(int32 value)

    member this.ArrayShape(shape: ArrayShape) =
        this.CompressedUnsigned shape.Rank
        this.CompressedUnsigned shape.Sizes.Length // NumSizes
        for size in shape.Sizes do this.CompressedUnsigned size
        this.CompressedUnsigned shape.LowerBounds.Length // NumLoBounds
        for bound in shape.LowerBounds do this.CompressedSigned bound // LoBound

    /// <summary>Writes a compact index into the <c>TypeDef</c>, <c>TypeRef</c>, or <c>TypeSpec</c> tables (II.23.2.8)</summary>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="index"/> cannot fit into 3 bytes.
    /// </exception>
    member this.TypeDefOrRefOrSpecEncoded(table, index) =
        if index > 0xFFFFFFu then
            ArgumentOutOfRangeException("index", index, "The type index must be able to fit into 3 bytes") |> raise
        this.CompressedUnsigned(table ||| (index <<< 2))

    member this.TypeDefOrRefOrSpecEncoded(index: TypeDefOrRefOrSpecEncoded) =
        this.TypeDefOrRefOrSpecEncoded(uint32 index.Tag, uint32 index.Value)

    member this.GenericInst(inst: GenericInst) =
        this.Writer.WriteU1 ElementType.GenericInst
        this.Writer.WriteU1(if inst.IsValueType then ElementType.ValueType else ElementType.Class)
        this.TypeDefOrRefOrSpecEncoded inst.Type
        this.CompressedUnsigned inst.GenericArguments.Length
        for garg in inst.GenericArguments do this.EncodedType garg

    member this.EncodedType(item: EncodedType) =
        match item with
        | EncodedType.Array(element, shape) ->
            this.Writer.WriteU1 ElementType.Array
            this.EncodedType element
            this.ArrayShape shape
        | EncodedType.Boolean -> this.Writer.WriteU1 ElementType.Boolean
        | EncodedType.Char -> this.Writer.WriteU1 ElementType.Char
        | EncodedType.Class item ->
            this.Writer.WriteU1 ElementType.Class
            this.TypeDefOrRefOrSpecEncoded item

        | EncodedType.GenericInst inst -> this.GenericInst inst

        | EncodedType.I -> this.Writer.WriteU1 ElementType.I
        | EncodedType.I1 -> this.Writer.WriteU1 ElementType.I1
        | EncodedType.I2 -> this.Writer.WriteU1 ElementType.I2
        | EncodedType.I4 -> this.Writer.WriteU1 ElementType.I4
        | EncodedType.I8 -> this.Writer.WriteU1 ElementType.I8

        | EncodedType.MVar num ->
            this.Writer.WriteU1 ElementType.MVar
            this.CompressedUnsigned num

        | EncodedType.Object -> this.Writer.WriteU1 ElementType.Object

        | EncodedType.String -> this.Writer.WriteU1 ElementType.String
        | EncodedType.SZArray(modifiers, element) ->
            this.Writer.WriteU1 ElementType.SZArray

            if not modifiers.IsEmpty then
                failwith "Custom modifiers for SZArray is not yet supported"

            this.EncodedType element


        | EncodedType.R4 -> this.Writer.WriteU1 ElementType.R4
        | EncodedType.R8 -> this.Writer.WriteU1 ElementType.R8
        | EncodedType.U1 -> this.Writer.WriteU1 ElementType.U1
        | EncodedType.U2 -> this.Writer.WriteU1 ElementType.U2
        | EncodedType.U4 -> this.Writer.WriteU1 ElementType.U4
        | EncodedType.U8 -> this.Writer.WriteU1 ElementType.U8

        | EncodedType.ValueType item ->
            this.Writer.WriteU1 ElementType.ValueType
            this.TypeDefOrRefOrSpecEncoded item

        | EncodedType.Var num ->
            this.Writer.WriteU1 ElementType.Var
            this.CompressedUnsigned num

        | bad -> failwithf "Unable to write unsupported type %A" bad

    member _.CustomMod(modifiers: #IReadOnlyCollection<CustomModifier>) =
        if modifiers.Count > 0 then
            failwith "TODO: Implement writing of custom modifiers"
            ()

    member this.RetType(item: ReturnTypeItem) =
        this.CustomMod item.CustomMod
        match item.ReturnType with
        | ReturnType.Void -> this.Writer.WriteU1 ElementType.Void
        | ReturnType.Type item -> this.EncodedType item
        | bad -> failwithf "Unable to write unsupported return type %A" bad

    member this.Parameters(items: ImmutableArray<ParamItem>) =
        for param in items do
            this.CustomMod param.CustomMod
            this.EncodedType param.ParamType

    member this.Elem(elem: Elem) =
        match elem with
        | ValBool true -> this.Writer.WriteU1 1uy
        | ValBool false -> this.Writer.WriteU1 0uy
        | SerString "" -> this.CompressedUnsigned 0u
        | SerString null -> this.CompressedUnsigned 0xFFu
        | SerString str ->
            let bytes = Encoding.UTF8.GetBytes str
            this.CompressedUnsigned bytes.Length // PackedLen
            this.Writer.WriteBytes bytes
        | bad -> failwithf "Unsupported element %A" bad

    member this.FixedArg(arg: FixedArg) =
        match arg with
        | FixedArg.Elem elem -> this.Elem elem
        | FixedArg.SZArray _ -> failwith "TODO: Implement writing of SZArray for FixedArg"

    member this.FieldSig(signature: FieldSignature) =
        this.Writer.WriteU1 0x6uy // FIELD
        this.CustomMod signature.CustomMod
        this.EncodedType signature.FieldType
end
