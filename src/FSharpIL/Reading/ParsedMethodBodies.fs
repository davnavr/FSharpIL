namespace FSharpIL.Reading

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL
open FSharpIL.Utilities

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
      SectionDataOffset: uint32
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
    | MethodBodyOutOfSection of offset: uint32
    | InvalidFatMethodHeaderSize of size: uint16
    | MissingDataSectionFlags of offset: uint32
    | InvalidDataSectionFlags of ILSectionFlags
    | MissingDataSectionSize of offset: uint32 * int32
    | InvalidDataSectionSize of ILSectionFlags * offset: uint32 * expected: uint32 * actual: uint32
    | DataSectionOutOfBounds of offset: uint32 * size: uint32

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
            sprintf "The method body at offset 0x%08X from the start of the section is out of bounds" offset
        | InvalidFatMethodHeaderSize size ->
            sprintf "Expected fat method header size of 3 * 4 = 12 bytes but got %i * 4 = %i bytes instead" size (size * 4us)
        | MissingDataSectionFlags offset ->
            sprintf "Expected method data section flags at offset 0x%08X from the start of the section" offset
        | InvalidDataSectionFlags flags -> sprintf "Invalid method data section flags (0x%02X) %A" (uint8 flags) flags
        | MissingDataSectionSize(offset, size) ->
            sprintf "Expected %i method data section size bytes at offset 0x%08X from the start of the section" size offset
        | InvalidDataSectionSize(flags, offset, expected, actual) ->
            sprintf
                "Expected method data section %A at offset 0x%08X to have a size of %i bytes, but got %i bytes instead"
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

    | Ldnull = 0x14us
    | Ldc_i4_m1 = 0x15us
    | Ldc_i4_0 = 0x16us
    | Ldc_i4_1 = 0x17us
    | Ldc_i4_2 = 0x18us
    | Ldc_i4_3 = 0x19us
    | Ldc_i4_4 = 0x1Aus
    | Ldc_i4_5 = 0x1Bus
    | Ldc_i4_6 = 0x1Cus
    | Ldc_i4_7 = 0x1Dus
    | Ldc_i4_8 = 0x1Eus

    | Dup = 0x25us
    | Pop = 0x26us

    | Ret = 0x2Aus

    | Add = 0x58us
    | Sub = 0x59us
    | Mul = 0x5Aus
    | Div = 0x5Bus
    | Div_un = 0x5Bus
    | Rem = 0x5Dus
    | Rem_un = 0x5Eus
    | And = 0x5Fus
    | Or = 0x60us
    | Xor = 0x61us
    | Shl = 0x62us
    | Shr = 0x63us
    | Shr_un = 0x64us
    | Neg = 0x65us
    | Not = 0x66us
    | Conv_i1 = 0x67us
    | Conv_i2 = 0x68us
    | Conv_i4 = 0x69us
    | Conv_i8 = 0x6Aus
    | Conv_r4 = 0x6Bus
    | Conv_r8 = 0x6Cus
    | Conv_u4 = 0x6Dus
    | Conv_u8 = 0x6Eus

    | Conv_r_un = 0x76us

    | Throw = 0x7Aus

    | Conv_ovf_i1_un = 0x82us
    | Conv_ovf_i2_un = 0x83us
    | Conv_ovf_i4_un = 0x84us
    | Conv_ovf_i8_un = 0x85us
    | Conv_ovf_u1_un = 0x86us
    | Conv_ovf_u2_un = 0x87us
    | Conv_ovf_u4_un = 0x88us
    | Conv_ovf_u8_un = 0x89us
    | Conv_ovf_i_un = 0x8Aus
    | Conv_ovf_u_un = 0x8Bus

    | Ldlen = 0x8Eus

    | Conv_ovf_i1 = 0xB3us
    | Conv_ovf_u1 = 0xB4us
    | Conv_ovf_i2 = 0xB5us
    | Conv_ovf_u2 = 0xB6us
    | Conv_ovf_i4 = 0xB7us
    | Conv_ovf_u4 = 0xB8us
    | Conv_ovf_i8 = 0xB9us
    | Conv_ovf_u8 = 0xBAus

    | Ckfinite = 0xC3us

    | Conv_u2 = 0xD1us
    | Conv_u1 = 0xD2us
    | Conv_i = 0xD3us
    | Conv_ovf_i = 0xD4us
    | Conv_ovf_u = 0xD5us
    | Add_ovf = 0xD6us
    | Add_ovf_un = 0xD7us
    | Mul_ovf = 0xD8us
    | Mul_ovf_un = 0xD9us
    | Sub_ovf = 0xDAus
    | Sub_ovf_un = 0xDBus
    //| Endfault = 0xDCus
    | Endfinally = 0xDCus

    | Conv_u = 0xE0us

    | Arglist = 0xFE_00us
    | Ceq = 0xFE_01us
    | Cgt = 0xFE_02us
    | Cgt_un = 0xFE_03us
    | Clt = 0xFE_04us
    | Clt_un = 0xFE_05us

    | Localloc = 0xFE_0Fus

    | Endfilter = 0xFE_11us

    | Volatile_ = 0xFE_13us
    | Tail_ = 0xFE_14us

    | Cpblk = 0xFE_17us
    | Initblk = 0xFE_18us

    | Rethrow = 0xFE_1Aus

    | Refanytype = 0xFE_1Dus
    | Readonly_ = 0xFE_1Eus


[<RequireQualifiedAccess>]
module ParsedOpcode =
    // TODO: Consider caching the names.
    let name (opcode: ParsedOpcode) = opcode.ToString().ToLowerInvariant().Replace('_', '.')

[<NoComparison>]
type InvalidOpcode =
    | UnexpectedEndOfBody of offset: uint32
    | UnknownOpcode of offset: uint32 * opcode: uint8
    | MissingOperandBytes of offset: uint32 * count: uint32

    override this.ToString() =
        match this with
        | UnexpectedEndOfBody offset -> sprintf "Unexpected end of method body at offset IL_%04x" offset
        | UnknownOpcode(offset, opcode) -> sprintf "Unknown opcode (0x%02X) at offset IL_%04x" opcode offset
        | MissingOperandBytes(offset, count) -> sprintf "Expected %i operand bytes at offset IL_%04x" count offset

type ParsedOperandTag =
    | None = 0uy
    | U1 = 1uy
    | U2 = 2uy
    | I1 = 5uy
    | I4 = 7uy
    | MetadataToken = 9uy
    | SwitchTargets = 10uy

[<IsReadOnly; IsByRefLike; Struct>]
type ParsedOperand internal (tag: ParsedOperandTag, operand: ChunkedMemory) =
    member _.Tag = tag
    member internal _.Operand = operand // TODO: Consider storing pointer (if possible) to operand bytes instead.

// TODO: Have enum for table type.
type [<IsReadOnly; Struct>] ParsedMetadataToken = { Table: uint8; Index: uint32 }

[<RequireQualifiedAccess>]
module ParsedOperand =
    let inline private (|Bytes|) (operand: ParsedOperand) = operand.Operand
    let u1 (Bytes buffer) = buffer.[0u]
    let u2 (Bytes buffer) = ChunkedMemory.readU2 0u &buffer
    let u4 (Bytes buffer) = ChunkedMemory.readU4 0u &buffer
    let metadataToken operand =
        let token = u4 operand
        { Table = Checked.uint8(token >>> 24); Index = token &&& 0xFFFFFFu }
    let switchTargets (Bytes buffer) =
        // The number of targets is excluded, only the targets are stored in the buffer.
        let ntargets = buffer.Length / 4u
        let mutable counts = Array.zeroCreate<uint32>(int32 ntargets)
        let mutable i = 0u
        while i < ntargets do
            counts.[int32 i ] <- ChunkedMemory.readU4 (i * 4u) &buffer
            i <- i  + 1u
        Unsafe.As<_, ImmutableArray<int32>> &counts

    // TODO: How to differentiate between normal integer operands and jump targets?
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
type MethodBodyStream = // TODO: For method body stream, simply slice it.
    internal
        { Chunk: ChunkedMemory
          MethodOffset: uint32
          MethodSize: uint32 }

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
        this.Chunk.[this.MethodOffset + offset]
    member this.Size = this.MethodSize

[<RequireQualifiedAccess>]
module internal MethodBodyStream =
    let create (chunk: inref<ChunkedMemory>) offset size =
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
    val mutable private offset: uint32
    new (body) = { MethodBody = body; offset = 0u }

    member this.Offset = this.offset
    member inline private this.CanRead = this.offset < this.MethodBody.Size
    /// Offset from the start of the section to the current offset into the method body // TODO: Change this description if method body chunk was sliced.
    member inline private this.SectionOffset = this.MethodBody.MethodOffset + this.offset

    member private this.ReadByte(value: outref<byte>) =
        if this.CanRead then
            value <- this.MethodBody.[this.offset]
            this.offset <- this.offset + 1u
            true
        else false

    member private this.ReadOpcode(error: outref<InvalidOpcode>) =
        match this.ReadByte() with
        | false, _ -> ReachedEnd
        | _, 0xFEuy ->
            // TODO: Avoid code duplication with reading first byte.
            match this.ReadByte() with
            | false, _ -> ReachedEnd
            | _, Convert.U2 opcode when opcode <= 0x1Eus -> Success(LanguagePrimitives.EnumOfValue(0xFE00us ||| opcode))
            | _, unknown ->
                error <- UnknownOpcode(this.offset - 1u, unknown)
                Failure
        | _, Convert.U2 opcode when (opcode <= 0xEFus || opcode >= 0xFCus) && Enum.IsDefined(typeof<ParsedOpcode>, opcode) -> // TODO: Once all opcodes have been included in the enum, remove the Enum.IsDefined call for performance reasons.
            Success(LanguagePrimitives.EnumOfValue opcode)
        | _, unknown ->
            error <- UnknownOpcode(this.offset - 1u, unknown)
            Failure

    member private this.ReadOperandBytes(tag, size, operand: outref<ParsedOperand>, error: outref<InvalidOpcode>) =
        let success, operand' = this.MethodBody.Chunk.TrySlice(this.SectionOffset, size)
        if success then
            operand <- ParsedOperand(tag, operand')
            this.offset <- this.offset + size
        else error <- MissingOperandBytes(this.offset, size)
        success

    member private this.ReadOperand(opcode, operand: outref<_>, error: outref<_>) =
        match opcode with
        //| SomeOtherOpcode
        | ParsedOpcode.Ldarg_s -> this.ReadOperandBytes(ParsedOperandTag.U1, 1u, &operand, &error)
        | _ ->
            operand <- ParsedOperand()
            true

    /// <param name="operand">
    /// When this method returns, contains the operand of the opcode that was parsed.
    /// </param>
    /// <returns>
    /// The number of bytes read and the opcode that was parsed, or an error indicating how the method body is invalid.
    /// </returns>
    member this.Read(operand: outref<ParsedOperand>) =
        let start = this.offset
        match this.ReadOpcode() with
        | Success opcode, _ ->
            match this.ReadOperand(opcode, &operand) with
            | true, _ -> struct(this.offset - start, Ok opcode)
            | false, err -> struct(0u, Error err)
        | ReachedEnd, _ -> struct(0u, Ok ParsedOpcode.Nop)
        | Failure, err -> struct(0u, Error err)
end

type MethodBodyStream with member this.Parse() = MethodBodyParser this

[<ReferenceEquality>]
type ParsedMethodBodies =
    internal
        { Chunk: ChunkedMemory
          /// RVA of the section containing the method bodies.
          SectionRva: uint32 }

    member private this.TryParseHeader(offset: uint32) =
        let start = this.Chunk.[offset]
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
                let buffer = Span.stackalloc<byte> 12 // TODO: Don't use buffer for method body header
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
        let rec inner (offset: uint32) =
            if this.Chunk.IsValidOffset offset then
                let flags: ILSectionFlags = LanguagePrimitives.EnumOfValue(this.Chunk.[offset])
                let buffer = Span.stackalloc<byte>(if flags.HasFlag ILSectionFlags.FatFormat then 3 else 1) // TODO: Don't use span
                if this.Chunk.TryCopyTo(offset + 1u, buffer) then
                    let section =
                        { SectionType = flags
                          SectionDataOffset = offset + 1u + uint32 buffer.Length
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
                        let offset' = Round.upTo 4u (section.SectionDataOffset + size)
                        if flags.HasFlag ILSectionFlags.MoreSects
                        then inner offset'
                        else Ok(struct(offset', sections.ToImmutable()))
                    | Unknown _ -> Error(InvalidDataSectionFlags flags)
                else Error(MissingDataSectionSize(offset, buffer.Length))
            else Error(MissingDataSectionFlags offset)
        inner(Round.upTo 4u hoffset)

    member private this.TryParseTinyEHTable(parser: #IMethodDataSectionParser, offset: uint32, count: int32) =
        for i = 0 to count - 1 do
            let offset' = offset + (uint32 i * 12u)
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
                    this.TryParseTinyEHTable(parser, section.SectionDataOffset + 2u, int32((size - 2u) / 12u))
                    inner (i + 1)
                | FatEHTable size ->
                    //failwith "TODO: Parse fat EH table"
                    inner (i + 1)
                | Unknown flags -> Error(InvalidDataSectionFlags flags)
            else Ok parser
        inner 0

    /// Attempts to parse the method body at the specified relative virtual address.
    member this.TryParseBody(rva: uint32) =
        let offset = rva - this.SectionRva
        if this.Chunk.IsValidOffset offset then
            let header = this.TryParseHeader offset
            match header with
            | Ok header' ->
                let moffset =
                    let offset' =
                        match header'.Size with
                        | 0uy -> 1u
                        | Convert.U4 hsize -> hsize * 4u
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
                          MethodSize = header'.CodeSize }
                    )
                | Error err -> Error err
            | Error err -> Error err
        else Error(InvalidMethodBodyRva rva)
