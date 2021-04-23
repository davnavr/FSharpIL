/// Contains functions for reading CLI metadata in the file format described by the ECMA-335 standard (II.25).
[<RequireQualifiedAccess>]
module FSharpIL.ReadCli

open System
open System.IO

open Microsoft.FSharp.NativeInterop

open FSharpIL.Reading

#nowarn "9"

let inline private spanalloc<'T when 'T : unmanaged> count = Span<'T>(NativePtr.toVoidPtr(NativePtr.stackalloc<'T> count), count)

let rec private readPE offset (src: Stream) (reader: MetadataReader<_>) ustate state =
    match state with
    | ReadPEMagic ->
        let magic = spanalloc<byte> 4
        match src.Read magic with
        | 0 ->
            reader.HandleError ustate state UnexpectedEndOfFile
        | 4 when Magic.matches Magic.PESignature magic ->
            readPE (offset + 4u) src reader ustate MoveToLfanew
        | len ->
            InvalidPEMagic(magic.Slice(0, len).ToArray()) |> reader.HandleError ustate state
    | MoveToLfanew ->
        
        invalidOp ""
    | EndRead -> ustate

/// <remarks>The <paramref name="stream"/> is not disposed after reading is finished.</remarks>
/// <exception cref="System.ArgumentException">The <paramref name="stream"/> does not support reading.</exception>
let fromStream stream (state: 'State) reader: 'State = readPE 0u stream reader state ReadPEMagic
