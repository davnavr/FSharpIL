namespace FSharpIL.Reading

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL
open FSharpIL.Metadata

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ParsedMethodBodyHeader =
    { Flags: ILMethodFlags
      Size: uint8
      MaxStack: uint16
      CodeSize: uint32
      LocalVarSigTok: unit voption } // TODO: Have special value type for local variable token.
     // TODO: Include data sections

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
                if this.Chunk.TryReadBytes(offset, buffer) then
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
        let offset = uint64(this.SectionRva - rva)
        if this.Chunk.IsValidOffset offset then
            let header = this.TryParseHeader offset
            match header with
            | Ok header' ->
                if header'.Flags.HasFlag ILMethodFlags.MoreSects then
                    failwith "TODO: Read additional method data"
                failwith "Make type to parse body one byte at a time, and return it along with the header"
            | Error err -> Error err
        else Error(InvalidMethodBodyRva rva)
