[<AutoOpen>]
module internal FSharpIL.Metadata.Heaps.ChunkWriterExtensions

open System.Collections.Generic
open System.Text

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Metadata

open FSharpIL.Bytes
open FSharpIL.Writing

type ChunkWriter with
    /// <summary>Writes an unsigned compressed integer in big-endian order (II.23.2).</summary>
    /// <exception cref="System.ArgumentException">The <paramref name="value"/> is greater than the maximum compressed unsigned integer.</exception>
    member this.WriteCompressed(value: uint32) =
        match BlobSize.ofUnsigned value with
        | BlobSize.B4 ->
            // Sets bit 31 and bit 30, bit 29 remains clear
            let (U4 (msb, b2, b3, lsb)) = value ||| 0xC000_0000u
            this.WriteU1 msb
            this.WriteU1 b2
            this.WriteU1 b3
            this.WriteU1 lsb
        | BlobSize.B2 ->
            // Sets bit 15, bit 14 remains clear
            let (U2 (msb, lsb)) = uint16 (value ||| 0x8000u)
            this.WriteU1 msb
            this.WriteU1 lsb
        | BlobSize.B1 -> this.WriteU1 value // Bit 7 remains clear

    member this.WriteBlobSize(size: uint32) = // TODO: Figure out if blob size is same as an unsigned compressed integer.
        match BlobSize.ofUnsigned size with
        | BlobSize.B1 -> this.WriteU1 size // Bit 7 remains clear
        | _ -> failwith "TODO: Figure out how blob length is encoded in 2 or 4 bytes."

    member inline this.WriteCompressed value = this.WriteCompressed(uint32 value)

    member _.WriteCompressedSigned(value: int32) =
        failwith "TODO: Implement writing of compressed integers"
        ()

    member inline this.WriteCompressedSigned value = this.WriteCompressedSigned(int32 value)

    member this.WriteArrayShape(shape: ArrayShape) =
        this.WriteCompressed shape.Rank
        this.WriteCompressed shape.Sizes.Length // NumSizes
        for size in shape.Sizes do this.WriteCompressed size
        this.WriteCompressed shape.LowerBounds.Length // NumLoBounds
        for bound in shape.LowerBounds do this.WriteCompressedSigned bound // LoBound

    member this.WriteType(item: EncodedType) =
        match item with
        | EncodedType.Array(element, shape) ->
            this.WriteU1 ElementType.Array
            this.WriteType element
            this.WriteArrayShape shape

        | EncodedType.String -> this.WriteU1 ElementType.String

        | bad -> failwithf "Unsupported type %A" bad

    member _.WriteCustomMod(modifiers: #IReadOnlyCollection<CustomModifier>) =
        if modifiers.Count > 0 then
            failwith "TODO: Implement writing of custom modifiers"
            ()

    member this.WriteRetType(item: ReturnTypeItem) =
        this.WriteCustomMod item.CustomMod
        match item.ReturnType with
        | ReturnType.Void -> this.WriteU1 ElementType.Void

    member this.WriteParam(item: ParamItem) =
        this.WriteCustomMod item.CustomMod
        this.WriteType item.ParamType

    member this.WriteParameters(items: System.Collections.Immutable.ImmutableArray<ParamItem>) =
        for param in items do this.WriteParam param

    member this.WriteElem(elem: Elem) =
        match elem with
        | ValBool true -> this.WriteU1 1uy
        | ValBool false -> this.WriteU1 0uy
        | SerString "" -> this.WriteCompressed 0u
        | SerString null -> this.WriteCompressed 0xFFu
        | SerString str ->
            let bytes = Encoding.UTF8.GetBytes str
            this.WriteCompressed bytes.Length // PackedLen
            this.WriteBytes bytes
        | bad -> failwithf "Unsupported element %A" bad

    member this.WriteFixedArg(arg: FixedArg) =
        match arg with
        | FixedArg.Elem elem -> this.WriteElem elem
        | FixedArg.SZArray _ -> failwith "TODO: Implement writing of SZArray for FixedArg"
