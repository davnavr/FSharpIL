namespace FSharpIL.Reading

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL
open FSharpIL.Metadata

[<IsReadOnly; Struct>]
type ParsedMethodBodyHeader =
    { Flags: ILMethodFlags
      Size: uint8
      MaxStack: uint16
      CodeSize: uint32
      LocalVarSigTok: unit voption } // TODO: Have special value type for local variable token.
     // TODO: Include data sections

[<NoComparison>]
type MethodBodyError =
    | InvalidMethodBodyRva of rva: uint32
    | InvalidMethodBodyHeaderType of header: uint8
    | MethodBodyOutOfSection of offset: uint64
    | InvalidFatMethodHeaderSize of size: uint16

    override this.ToString() =
        match this with
        | InvalidMethodBodyRva rva -> sprintf "The method body RVA (0x%08X) is not in the correct section" rva
        | InvalidMethodBodyHeaderType header ->
            sprintf
                "The method body header type (0x%02X) is invalid, expected a tiny method header (0x%02X) or a fat method header (0x%02X)"
                header
                (uint8 ILMethodFlags.TinyFormat)
                (uint8 ILMethodFlags.FatFormat)
        | MethodBodyOutOfSection offset ->
            sprintf "The method body at offset 0x%016X from the start of the section is out of bounds" offset
        | InvalidFatMethodHeaderSize size ->
            sprintf "Expected fat method header size of 3 * 4 = 12 bytes but got %i * 4 = %i bytes instead" size (size * 4us)

type ParsedOpcode =
    | Nop = 0us
    | Break = 1us
    | Ldarg_0 = 2us
    | Ldarg_1 = 3us
    | Ldarg_2 = 4us
    | Ldarg_3 = 5us
    | Ldloc_0 = 6us
    | Ldloc_1 = 7us
    | Ldloc_2 = 8us
    | Ldloc_3 = 9us
    | Stloc_0 = 0xAus
    | Stloc_1 = 0xBus
    | Stloc_2 = 0xCus
    | Stloc_3 = 0xDus
    | Ldarg_s = 0xEus

[<NoComparison>]
type InvalidOpcode =
    | UnexpectedEndOfBody of offset: uint64
    | UnknownOpcode of offset: uint64 * opcode: uint8
    | MissingOperandBytes of offset: uint64 * count: int32

    override this.ToString() =
        match this with
        | UnexpectedEndOfBody offset -> sprintf "Unexpected end of method body at offset IL_0x%04x" offset
        | UnknownOpcode(offset, opcode) -> sprintf "Unknown opcode (%02X) at offset IL_0x%04x" opcode offset
        | MissingOperandBytes(offset, count) -> sprintf "Expected %i operand bytes at offset IL_0x%04x" count offset

type ParsedOperandTag =
    | None = 0uy
    | U1 = 1uy
    | U2 = 2uy
    | I1 = 5uy
    | I4 = 7uy
    | MetadataToken = 9uy
    | SwitchTargets = 10uy

[<IsReadOnly; IsByRefLike; Struct>]
type ParsedOperand internal (tag: ParsedOperandTag, operand: Span<byte>) =
    member _.Tag = tag
    member internal _.Bytes = operand

// TODO: Have enum for table type.
type [<IsReadOnly; Struct>] ParsedMetadataToken = { Table: uint8; Index: uint32 }

[<RequireQualifiedAccess>]
module ParsedOperand =
    let inline private (|Bytes|) (operand: ParsedOperand) = operand.Bytes
    let u1 (Bytes buffer) = buffer.[0]
    let u2 (Bytes buffer) = Bytes.readU2 0 buffer
    let u4 (Bytes buffer) = Bytes.readU4 0 buffer
    let inline metadataToken operand =
        let token = u4 operand
        { Table = Checked.uint8(token >>> 24); Index = token &&& 0xFFFFFFu }
    let switchTargets (Bytes buffer) =
        // The number of targets is excluded, only the targets are stored in the buffer.
        let mutable counts = Array.zeroCreate<uint32>(buffer.Length / 4)
        for i = 0 to counts.Length - 1 do counts.[i] <- Bytes.readU4 (i * 4) buffer
        Unsafe.As<_, ImmutableArray<int32>> &counts

    let inline (|None|U1|U2|I1|I4|MetadataToken|SwitchTargets|) (operand: ParsedOperand) =
        match operand.Tag with
        | ParsedOperandTag.None -> None
        | ParsedOperandTag.U1 -> U1(u1 operand)
        | ParsedOperandTag.U2 -> U2(u2 operand)
        | ParsedOperandTag.I1 -> I1(int8(u1 operand))
        | ParsedOperandTag.I4 -> I4(int32(u4 operand))
        | ParsedOperandTag.MetadataToken -> MetadataToken(metadataToken operand)
        | ParsedOperandTag.SwitchTargets -> SwitchTargets(switchTargets operand)
        | _ -> raise(ArgumentOutOfRangeException("operand", operand.Tag, "Invalid operand type"))

[<IsReadOnly; Struct>]
type MethodBodyStream =
    internal
        { Chunk: ChunkReader
          MethodOffset: uint64
          MethodSize: uint64 }

    /// <param name="offset">An offset from the start of this method.</param>
    /// <exception cref="System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="offset"/> is greater than or equal to the size of the method.
    /// </exception>
    member this.Item with get offset =
        if offset >= this.MethodSize then
            raise (
                ArgumentOutOfRangeException (
                    "offset",
                    offset,
                    sprintf "The method offset exceeds the size of the method (%i bytes)" this.MethodSize
                )
            )
        this.Chunk.ReadU1(this.MethodOffset + offset)
    member this.Size = this.MethodSize

[<RequireQualifiedAccess>]
module internal MethodBodyStream =
    let create (chunk: ChunkReader) offset size =
        if chunk.IsValidOffset(offset + size) then
            ValueSome
                { Chunk = chunk
                  MethodSize = size
                  MethodOffset = offset }
        else ValueNone

[<IsReadOnly; Struct>]
type internal OpcodeParseResult =
    | ReachedEnd
    | Failure
    | Success of ParsedOpcode

[<IsByRefLike; Struct>]
type MethodBodyParser = struct
    val MethodBody: MethodBodyStream
    val mutable private offset: uint64
    new (body) = { MethodBody = body; offset = 0UL }

    member inline private this.CanRead = this.offset < this.MethodBody.Size
    member inline private this.SectionOffset = this.MethodBody.MethodOffset + this.offset

    member private this.ReadByte(value: outref<byte>) =
        if this.CanRead then
            value <- this.MethodBody.[this.offset]
            this.offset <- this.offset + 1UL
            true
        else false

    member private this.ReadOpcode(error: outref<InvalidOpcode>) =
        match this.ReadByte() with
        | false, _ -> ReachedEnd
        | _, 0uy -> Success ParsedOpcode.Nop
        | _, unknown ->
            error <- UnknownOpcode(this.offset, unknown)
            Failure

    member private this.ReadOperandBytes(tag, size, operand: outref<ParsedOperand>, error: outref<InvalidOpcode>) =
        if this.MethodBody.Chunk.HasFreeBytes(this.SectionOffset, uint64 size) then
            operand <- ParsedOperand(tag, this.MethodBody.Chunk.ReadBytes(this.SectionOffset, size))
            true
        else
            error <- MissingOperandBytes(this.offset, size)
            false

    member private this.ReadOperand(opcode, operand: outref<_>, error: outref<_>) =
        match opcode with
        //| SomeOtherOpcode
        | ParsedOpcode.Ldarg_s -> this.ReadOperandBytes(ParsedOperandTag.U1, 1, &operand, &error)
        | _ ->
            operand <- ParsedOperand()
            true

    member this.Read(operand: outref<ParsedOperand>) =
        let start = this.offset
        match this.ReadOpcode() with
        | Success opcode, _ ->
            match this.ReadOperand(opcode, &operand) with
            | true, _ -> struct(this.offset - start, Ok opcode)
            | false, err -> struct(0UL, Error err)
        | ReachedEnd, _ -> struct(0UL, Ok ParsedOpcode.Nop)
        | Failure, err -> struct(0UL, Error err)
end

type MethodBodyStream with member this.Parse() = MethodBodyParser this

[<ReferenceEquality>]
type ParsedMethodBodies =
    internal
        { Chunk: ChunkReader
          /// RVA of the section containing the method bodies.
          SectionRva: uint32 }

    member private this.TryParseHeader(offset: uint64) =
        let start = this.Chunk.ReadU1 offset
        if start &&& 0b11uy > 1uy then
            let htype = LanguagePrimitives.EnumOfValue(uint16 start)
            match htype with
            | ILMethodFlags.TinyFormat ->
                Ok
                    { Flags = ILMethodFlags.TinyFormat
                      Size = 0uy
                      MaxStack = 8us // Max number of items allowed on the stack for tiny methods.
                      CodeSize = (uint32 start) >>> 2
                      LocalVarSigTok = ValueNone }
            | _ ->
                let buffer = Span.stackalloc<byte> 12
                if this.Chunk.TryCopyTo(offset, buffer) then
                    let flags = Bytes.readU2 0 buffer
                    match flags >>> 12 with
                    | 3us as size ->
                        Ok
                            { Flags = LanguagePrimitives.EnumOfValue(flags &&& 0xFFFus)
                              Size = uint8 size
                              MaxStack = Bytes.readU2 2 buffer
                              CodeSize = Bytes.readU4 4 buffer
                              LocalVarSigTok = Bytes.readU4 8 buffer |> failwith "TODO: Read local variable signature" }
                    | size -> Error(InvalidFatMethodHeaderSize size)
                else Error(MethodBodyOutOfSection offset)
        else Error(InvalidMethodBodyHeaderType start)

    /// Attempts to parse the method body at the specified relative virtual address.
    member this.TryParse(rva: uint32) =
        let offset = uint64(rva - this.SectionRva)
        if this.Chunk.IsValidOffset offset then
            let header = this.TryParseHeader offset
            match header with
            | Ok header' ->
                if header'.Flags.HasFlag ILMethodFlags.MoreSects then
                    failwith "TODO: Read additional method data"
                Ok struct (
                    header',
                    { Chunk = this.Chunk
                      MethodOffset =
                        match header'.Size with
                        | 0uy -> 1UL
                        | Convert.U8 hsize -> hsize * 4UL
                        + offset
                      MethodSize = uint64 header'.CodeSize }
                )
            | Error err -> Error err
        else Error(InvalidMethodBodyRva rva)
