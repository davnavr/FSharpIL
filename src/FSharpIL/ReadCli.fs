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
      mutable CoffHeader: CoffHeader<uint16, uint16>
      mutable StandardFields: StandardFields<PEImageKind, uint32, uint32 voption>
      mutable NTSpecificFields: NTSpecificFields<uint64, uint32 * uint32, uint32, uint64, uint32> }

let readCoffHeader (src: Reader) (headers: byref<_>) reader ustate =
    let buffer = arrspan<byte>(int32 WritePE.Size.CoffHeader)
    let len = src.ReadBytes buffer
    if len = buffer.Length then
        headers <-
            { Machine = LanguagePrimitives.EnumOfValue(Bytes.readU2 0 buffer)
              NumberOfSections = Bytes.readU2 2 buffer
              TimeDateStamp = Bytes.readU4 4 buffer
              SymbolTablePointer = Bytes.readU4 8 buffer
              SymbolCount = Bytes.readU4 12 buffer
              OptionalHeaderSize = Bytes.readU2 16 buffer
              Characteristics = LanguagePrimitives.EnumOfValue(Bytes.readU2 18 buffer) }
        MetadataReader.readCoffHeader reader headers ustate |> Ok
    else Error UnexpectedEndOfFile

let readStandardFields (src: Reader) (fields: byref<_>) reader ustate =
    let magic' = allocspan<byte> 2
    if src.ReadBytes magic' = 2 then
        let magic = LanguagePrimitives.EnumOfValue(Bytes.readU2 0 magic')
        let length = if magic = PEImageKind.PE32 then 26 else 22
        let fields' = arrspan<byte> length
        if src.ReadBytes fields' = length then
            fields <-
                { Magic = magic
                  LMajor = fields'.[0]
                  LMinor = fields'.[1]
                  CodeSize = Bytes.readU4 2 fields'
                  InitializedDataSize = Bytes.readU4 6 fields'
                  UninitializedDataSize = Bytes.readU4 10 fields'
                  EntryPointRva = Bytes.readU4 14 fields'
                  BaseOfCode = Bytes.readU4 18 fields'
                  BaseOfData =
                    match magic with
                    | PEImageKind.PE32 -> ValueSome(Bytes.readU4 22 fields')
                    | _ -> ValueNone }
            MetadataReader.readStandardFields
                reader
                fields
                ustate
            |> Ok
        else Error UnexpectedEndOfFile
    else Error UnexpectedEndOfFile

let readNTSpecificFields (src: Reader) magic (fields: byref<_>) reader ustate =
    let length =
        match magic with
        | PEImageKind.PE32Plus -> 88
        | _ -> 68
    let buffer = arrspan<byte> length
    if src.ReadBytes buffer = length then
        fields <-
            match magic with
            | PEImageKind.PE32Plus -> invalidOp "bad"
            | _ ->
                { ImageBase = uint64(Bytes.readU4 0 buffer)
                  // TODO: Validate alignment values, maybe use Alignment type?
                  Alignment = Bytes.readU4 4 buffer, Bytes.readU4 8 buffer
                  OSMajor = Bytes.readU2 12 buffer
                  OSMinor = Bytes.readU2 14 buffer
                  UserMajor = Bytes.readU2 16 buffer
                  UserMinor = Bytes.readU2 18 buffer
                  SubSysMajor = Bytes.readU2 20 buffer
                  SubSysMinor = Bytes.readU2 22 buffer
                  Win32VersionValue = Bytes.readU4 24 buffer
                  ImageSize = Bytes.readU4 28 buffer
                  HeadersSize = Bytes.readU4 32 buffer
                  FileChecksum = Bytes.readU4 36 buffer
                  Subsystem = LanguagePrimitives.EnumOfValue(Bytes.readU2 40 buffer)
                  DllFlags = LanguagePrimitives.EnumOfValue(Bytes.readU2 42 buffer)
                  StackReserveSize = uint64(Bytes.readU4 44 buffer)
                  StackCommitSize = uint64(Bytes.readU4 48 buffer)
                  HeapReserveSize = uint64(Bytes.readU4 52 buffer)
                  HeapCommitSize = uint64(Bytes.readU4 56 buffer)
                  LoaderFlags = Bytes.readU4 60 buffer
                  NumberOfDataDirectories = Bytes.readU4 64 buffer }
        MetadataReader.readNTSpecificFields reader fields ustate |> Ok
    else Error UnexpectedEndOfFile

let readPE (stream: Stream) (reader: MetadataReader<_>) (start: 'UserState) =
    let src = Reader stream
    let file =
        { Lfanew = Unchecked.defaultof<uint32>
          CoffHeader = Unchecked.defaultof<_>
          StandardFields = Unchecked.defaultof<_>
          NTSpecificFields = Unchecked.defaultof<_> }
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
            match readCoffHeader src &file.CoffHeader reader ustate with
            | Ok _ when file.CoffHeader.OptionalHeaderSize < WritePE.Size.OptionalHeader ->
                error(OptionalHeaderTooSmall file.CoffHeader.OptionalHeaderSize)
            | Ok ustate' -> inner ustate' ReadStandardFields
            | Error err -> error err
        | ReadStandardFields ->
            match readStandardFields src &file.StandardFields reader ustate with
            | Ok ustate' -> inner ustate' ReadNTSpecificFields
            | Error err -> error err
        | ReadNTSpecificFields ->
            match readNTSpecificFields src file.StandardFields.Magic &file.NTSpecificFields reader ustate with
            | Ok ustate' -> inner ustate' ReadDataDirectories
            | Error err -> error err
        | EndRead -> ustate
    inner start ReadPEMagic

/// <remarks>The <paramref name="stream"/> is not disposed after reading is finished.</remarks>
/// <exception cref="System.ArgumentException">The <paramref name="stream"/> does not support reading.</exception>
let fromStream stream state reader = readPE stream reader state
