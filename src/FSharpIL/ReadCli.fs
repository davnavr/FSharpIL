/// Contains functions for reading CLI metadata in the file format described by the ECMA-335 standard (II.25).
[<RequireQualifiedAccess>]
module FSharpIL.ReadCli

open System
open System.IO

open Microsoft.FSharp.Core.Operators.Checked
open Microsoft.FSharp.NativeInterop

open FSharpIL.Reading

#nowarn "9"

[<RequiresExplicitTypeArguments>]
let inline spanalloc<'T when 'T : unmanaged> count = Span<'T>(NativePtr.toVoidPtr(NativePtr.stackalloc<'T> count), count)

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
        let buf = spanalloc<byte> 1
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
        let bytes = spanalloc<byte> 4
        match this.ReadBytes bytes with
        | 4 ->
            value <-
                (uint32 bytes.[3] <<< 24) /// msb
                ||| (uint32 bytes.[2] <<< 16)
                ||| (uint32 bytes.[1] <<< 8)
                ||| uint32 bytes.[0] /// lsb
            true
        | _ -> false

[<NoComparison; NoEquality>]
type File =
    { mutable Lfanew: uint32 }

let readPE (stream: Stream) (reader: MetadataReader<_>) (start: 'UserState) =
    let src = Reader stream
    let file =
        { Lfanew = Unchecked.defaultof<_> }
    let rec inner ustate state =
        match state with
        | ReadPEMagic ->
            let magic = spanalloc<byte> 2
            match src.ReadBytes magic with
            | 0 ->
                reader.HandleError ustate state UnexpectedEndOfFile
            | 2 when Magic.matches Magic.PE magic ->
                inner ustate MoveToLfanew
            | len ->
                let magic' = magic.Slice(0, len).ToArray()
                reader.HandleError ustate state (InvalidPEMagic magic')
        | MoveToLfanew ->
            if src.MoveTo 0x3CUL
            then inner ustate ReadLfanew
            else reader.HandleError ustate state UnexpectedEndOfFile
        | ReadLfanew ->
            if src.ReadU4(&file.Lfanew) then
                let ustate' =
                    match reader.ReadLfanew with
                    | ValueSome read -> read file.Lfanew ustate
                    | ValueNone -> ustate
                inner ustate' MoveToPEFileHeader
            else reader.HandleError ustate state UnexpectedEndOfFile
        | MoveToPEFileHeader ->
            if src.MoveTo(uint64 file.Lfanew)
            then inner ustate ReadCoffHeader
            else reader.HandleError ustate state UnexpectedEndOfFile
        // Don't forget to check PE32+
        | EndRead -> ustate
    inner start ReadPEMagic

/// <remarks>The <paramref name="stream"/> is not disposed after reading is finished.</remarks>
/// <exception cref="System.ArgumentException">The <paramref name="stream"/> does not support reading.</exception>
let fromStream stream state reader = readPE stream reader state
