/// Contains functions for reading CLI metadata in the file format described by the ECMA-335 standard (II.25).
[<RequireQualifiedAccess>]
module FSharpIL.ReadCli

open System
open System.IO

open Microsoft.FSharp.Core.Operators.Checked
open Microsoft.FSharp.NativeInterop

open FSharpIL.PortableExecutable
open FSharpIL.Reading

#nowarn "9"

/// <summary>Creates a <see cref="System.Span`1"/> from a region of memory allocated on the stack.</summary>
[<RequiresExplicitTypeArguments>]
let inline allocspan<'T when 'T : unmanaged> length = Span<'T>(NativePtr.toVoidPtr(NativePtr.stackalloc<'T> length), length)

[<RequiresExplicitTypeArguments>]
let inline arrspan<'T> length = Span<'T>(Array.zeroCreate<'T> length)

[<Sealed>]
type Reader (src: Stream) =
    let mutable pos = 0UL
    do if not src.CanRead then invalidArg "src" "The stream must support reading"

    member _.Offset = pos

    member _.ReadByte() =
        pos <- pos + 1UL
        src.ReadByte()

    member _.ReadBytes(buffer: Span<byte>) =
        let read = src.Read buffer
        pos <- pos + uint64 read
        read

    member _.SkipBytes(count: uint64) =
        let buf = allocspan<byte> 1
        let mutable cont, skipped = true, 0UL
        while cont && skipped < count do
            match src.Read buf with
            | 0 -> cont <- false
            | _ -> skipped <- skipped + 1UL
        pos <- pos + skipped
        skipped

    member this.MoveTo(offset: uint64) =
        if offset < pos then
            invalidArg "offset" "Cannot move to a previous location"
        elif offset = pos then
            true
        else
            let diff = offset - pos
            this.SkipBytes diff = diff

    /// Reads an unsigned, little-endian, 4-byte integer.
    member inline this.ReadU4(value: outref<uint32>) =
        let bytes = allocspan<byte> 4
        match this.ReadBytes bytes with
        | 4 ->
            value <- Bytes.readU4 0 bytes
            true
        | _ -> false

[<NoComparison; NoEquality>]
type MutableFile =
    { mutable Lfanew: uint32
      mutable OptionalHeaderSize: uint16
      mutable Magic: OptionalHeaderMagic }

let readCoffHeader (src: Reader) (file: MutableFile) (reader: MetadataReader<'UserState>) ustate =
    let buffer = arrspan<byte>(int32 WritePE.Size.CoffHeader)
    let len = src.ReadBytes buffer
    if len = buffer.Length then
        file.OptionalHeaderSize <- Bytes.readU2 16 buffer
        MetadataReader.readCoffHeader
            reader
            { Machine = LanguagePrimitives.EnumOfValue(Bytes.readU2 0 buffer)
              TimeDateStamp = Bytes.readU4 4 buffer
              SymbolTablePointer = Bytes.readU4 8 buffer
              SymbolCount = Bytes.readU4 12 buffer
              Characteristics = LanguagePrimitives.EnumOfValue(Bytes.readU2 18 buffer) }
            (Bytes.readU2 2 buffer) // NumberOfSections
            file.OptionalHeaderSize
            ustate
        |> Ok
    else Error UnexpectedEndOfFile

let readStandardFields (src: Reader) (magic: byref<OptionalHeaderMagic>) (reader: MetadataReader<'UserState>) ustate =
    let magic' = allocspan<byte> 2
    if src.ReadBytes magic' = 2 then
        magic <- LanguagePrimitives.EnumOfValue(Bytes.readU2 0 magic')
        let length = if magic = OptionalHeaderMagic.PE32 then 26 else 22 // TODO: Return an Error if the magic value is not PE332 or PE32+.
        let fields = arrspan<byte> length
        if src.ReadBytes fields = length then
            let baseOfData =
                match magic with
                | OptionalHeaderMagic.PE32 -> ValueSome(Bytes.readU4 24 fields)
                | _ -> ValueNone
            MetadataReader.readStandardFields
                reader
                magic
                fields.[2] // LMajor
                fields.[3] // LMinor
                (Bytes.readU4 4 fields) // CodeSize
                (Bytes.readU4 8 fields) // InitializedDataSize
                (Bytes.readU4 12 fields) // UninitializedDataSize
                (Bytes.readU4 16 fields) // EntryPointRva
                (Bytes.readU4 20 fields) // BaseOfCode
                baseOfData
                ustate
            |> Ok
        else Error UnexpectedEndOfFile
    else Error UnexpectedEndOfFile

let readPE (stream: Stream) (reader: MetadataReader<_>) (start: 'UserState) =
    let src = Reader stream
    let file =
        { Lfanew = Unchecked.defaultof<uint32>
          OptionalHeaderSize = Unchecked.defaultof<uint16>
          Magic = Unchecked.defaultof<OptionalHeaderMagic> }
    let rec inner ustate state =
        let inline error err = reader.HandleError src.Offset state err ustate
        let inline moveto target state' =
            if src.MoveTo target
            then inner ustate state'
            else error UnexpectedEndOfFile
        match state with
        | ReadPEMagic ->
            let magic = allocspan<byte> 2
            match src.ReadBytes magic with
            | 0 -> error UnexpectedEndOfFile
            | 2 when Magic.matches Magic.MZ magic -> inner ustate MoveToLfanew
            | len -> InvalidMagic(Magic.MZ, Bytes.ofSpan len magic) |> error
        | MoveToLfanew -> moveto 0x3CUL ReadLfanew
        | ReadLfanew ->
            if src.ReadU4(&file.Lfanew) then
                let ustate' = MetadataReader.readLfanew reader file.Lfanew ustate
                inner ustate' MoveToPESignature
            else error UnexpectedEndOfFile
        | MoveToPESignature -> moveto (uint64 file.Lfanew) ReadPESignature
        | ReadPESignature ->
            let signature = allocspan<byte> 4
            match src.ReadBytes signature with
            | 0 -> error UnexpectedEndOfFile
            | 4 when Magic.matches Magic.PESignature signature -> inner ustate ReadCoffHeader
            | len -> InvalidMagic(Magic.PESignature, Bytes.ofSpan len signature) |> error
        | ReadCoffHeader ->
            match readCoffHeader src file reader ustate with
            | Ok _ when file.OptionalHeaderSize < WritePE.Size.OptionalHeader ->
                error(OptionalHeaderTooSmall file.OptionalHeaderSize)
            | Ok ustate' -> inner ustate' ReadStandardFields
            | Error err -> error err
        | ReadStandardFields ->
            match readStandardFields src &file.Magic reader ustate with
            | Ok ustate' -> inner ustate' ReadNTSpecificFields
            | Error err -> error err
        // TODO: Don't forget to check PE32+
        | EndRead -> ustate
    inner start ReadPEMagic

/// <remarks>The <paramref name="stream"/> is not disposed after reading is finished.</remarks>
/// <exception cref="System.ArgumentException">The <paramref name="stream"/> does not support reading.</exception>
let fromStream stream state reader = readPE stream reader state
