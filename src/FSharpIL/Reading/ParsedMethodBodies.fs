namespace FSharpIL.Reading

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL
open FSharpIL.Metadata

/// II.25.4.6
[<IsReadOnly; Struct>]
type ParsedEHClause<'Offset, 'Length> =
    { Flags: ExceptionClauseFlags
      TryOffset: 'Offset
      TryLength: 'Length
      HandlerOffset: 'Offset
      HandlerLength: 'Length
      ClassToken: uint32 // TODO: Have type for metadata token used in eh clause.
      FilterOffset: uint32 }

[<IsReadOnly; Struct>]
type ParsedMethodDataSection =
    { SectionType: ILSectionFlags
      /// Offset from the start of the section to the first byte of the method section data.
      SectionDataOffset: uint64
      SectionSize: uint32 }

// TODO: Decide if this module should be public, autoopen
[<AutoOpen>]
module ParsedMethodDataSection =
    let inline (|Unknown|TinyEHTable|FatEHTable|) { SectionType = flags; SectionSize = size } =
        match flags &&& (~~~ILSectionFlags.MoreSects) with
        | ILSectionFlags.EHTable -> TinyEHTable((size * 12u) + 4u)
        | ILSectionFlags.FatEHTable -> FatEHTable((size * 24u) + 4u)
        | _ -> Unknown flags

type ParsedTinyEHClause = ParsedEHClause<uint16, uint8>
type ParsedFatEHClause = ParsedEHClause<uint32, uint32>

type [<Interface>] IMethodDataSectionParser =
    abstract TinyEHClause: ParsedTinyEHClause -> unit
    abstract FatEHClause: ParsedFatEHClause -> unit

[<IsReadOnly; Struct>]
type ParsedMethodBodyHeader =
    { Flags: ILMethodFlags
      Size: uint8
      MaxStack: uint16
      CodeSize: uint32
      LocalVarSigTok: unit voption } // TODO: Have special value type for local variable token.

[<NoComparison>]
type MethodBodyError =
    | InvalidMethodBodyRva of rva: uint32
    | InvalidMethodBodyHeaderType of header: uint8
    | MethodBodyOutOfSection of offset: uint64
    | InvalidFatMethodHeaderSize of size: uint16
    | MissingDataSectionFlags of offset: uint64
    | InvalidDataSectionFlags of ILSectionFlags
    | MissingDataSectionSize of offset: uint64 * int32
    | InvalidDataSectionSize of ILSectionFlags * offset: uint64 * expected: uint32 * actual: uint32
    | DataSectionOutOfBounds of offset: uint64 * size: uint64

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
        | MissingDataSectionFlags offset ->
            sprintf "Expected method data section flags at offset 0x%016X from the start of the section" offset
        | InvalidDataSectionFlags flags -> sprintf "Invalid method data section flags (0x%02X) %A" (uint8 flags) flags
        | MissingDataSectionSize(offset, size) ->
            sprintf "Expected %i method data section size bytes at offset 0x%016X from the start of the section" size offset
        | InvalidDataSectionSize(flags, offset, expected, actual) ->
            sprintf
                "Expected method data section %A at offset 0x%016X to have a size of %i bytes, but got %i bytes instead"
                flags
                offset
                expected
                actual

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
        let htype = start &&& 0b11uy
        if htype > 1uy then
            match LanguagePrimitives.EnumOfValue(uint16 htype) with
            | ILMethodFlags.TinyFormat ->
                Ok
                    { Flags = ILMethodFlags.TinyFormat
                      Size = 0uy
                      MaxStack = 8us // Max number of items allowed on the stack for tiny methods.
                      CodeSize = (uint32 start) >>> 2
                      LocalVarSigTok = ValueNone }
            | ILMethodFlags.FatFormat
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
                              LocalVarSigTok =
                                Bytes.readU4 8 buffer
                                ValueNone } // TODO: Read local variable signature
                    | size -> Error(InvalidFatMethodHeaderSize size)
                else Error(MethodBodyOutOfSection offset)
        else Error(InvalidMethodBodyHeaderType start)

    // TODO: Properly implement parsing of method data sections.

    member private this.TryFindDataSections hoffset =
        let sections = ImmutableArray.CreateBuilder<ParsedMethodDataSection>()
        let rec inner (offset: uint64) =
            if this.Chunk.IsValidOffset offset then
                let flags: ILSectionFlags = LanguagePrimitives.EnumOfValue(this.Chunk.ReadU1 offset)
                let buffer = Span.stackalloc<byte>(if flags.HasFlag ILSectionFlags.FatFormat then 3 else 1)
                if this.Chunk.TryCopyTo(offset + 1UL, buffer) then
                    let section =
                        { SectionType = flags
                          SectionDataOffset = offset + 1UL + uint64 buffer.Length
                          SectionSize =
                            match buffer.Length with
                            | 3 ->
                                (uint32 buffer.[2] <<< 16)
                                ||| (uint32 buffer.[1] <<< 8)
                                ||| uint32 buffer.[0]
                            | 1
                            | _ -> uint32 buffer.[0] }

                    match section with
                    | TinyEHTable size
                    | FatEHTable size ->
                        sections.Add section
                        // TODO: Check that the data is in bounds.
                        let offset' = Round.upTo 4UL (section.SectionDataOffset + uint64 size)
                        if flags.HasFlag ILSectionFlags.MoreSects
                        then inner offset'
                        else Ok(struct(offset', sections.ToImmutable()))
                    | Unknown _ -> Error(InvalidDataSectionFlags flags)
                else Error(MissingDataSectionSize(offset, buffer.Length))
            else Error(MissingDataSectionFlags offset)
        inner(Round.upTo 4UL hoffset)

    member private this.TryParseTinyEHTable(parser: #IMethodDataSectionParser, offset: uint64, count: int32) =
        for i = 0 to count - 1 do
            let offset' = offset + (uint64 i * 12UL)
            ()

    member this.TryParseDataSections<'Parser when 'Parser :> IMethodDataSectionParser> // TODO: Don't use 'Parser
        (
            parser: 'Parser,
            sections: ImmutableArray<ParsedMethodDataSection>
        ) =
        let rec inner i =
            if i < sections.Length then
                let section = sections.[i]
                match section with
                | TinyEHTable size ->
                    this.TryParseTinyEHTable(parser, section.SectionDataOffset + 2UL, int32((size - 2u) / 12u))
                    inner (i + 1)
                | FatEHTable size ->
                    //failwith "TODO: Parse fat EH table"
                    inner (i + 1)
                | Unknown flags -> Error(InvalidDataSectionFlags flags)
            else Ok parser
        inner 0

    /// Attempts to parse the method body at the specified relative virtual address.
    member this.TryParseBody(rva: uint32) =
        let offset = uint64(rva - this.SectionRva)
        if this.Chunk.IsValidOffset offset then
            let header = this.TryParseHeader offset
            match header with
            | Ok header' ->
                let moffset =
                    let offset' =
                        match header'.Size with
                        | 0uy -> 1UL
                        | Convert.U8 hsize -> hsize * 4UL
                        + offset
                    if header'.Flags.HasFlag ILMethodFlags.MoreSects
                    then this.TryFindDataSections offset'
                    else Ok(struct(offset', ImmutableArray.Empty))
                match moffset with
                | Ok(moffset', sections) ->
                    Ok struct (
                        header',
                        sections,
                        { Chunk = this.Chunk
                          MethodOffset = moffset'
                          MethodSize = uint64 header'.CodeSize }
                    )
                | Error err -> Error err
            | Error err -> Error err
        else Error(InvalidMethodBodyRva rva)
