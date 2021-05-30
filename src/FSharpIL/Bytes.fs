[<System.Obsolete>]
module internal FSharpIL.Bytes

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices
open System.Text

let readU2 offset (bytes: Span<byte>) =
    (uint16 bytes.[offset + 1] <<< 8) // MSB
    ||| uint16 bytes.[offset] // LSB

let readU4 offset (bytes: Span<byte>) =
    (uint32 bytes.[offset + 3] <<< 24) // MSB
    ||| (uint32 bytes.[offset + 2] <<< 16)
    ||| (uint32 bytes.[offset + 1] <<< 8)
    ||| uint32 bytes.[offset] // LSB

let readU8 offset (bytes: Span<byte>) =
    (uint64 bytes.[offset + 7] <<< 56) // MSB
    ||| (uint64 bytes.[offset + 6] <<< 48)
    ||| (uint64 bytes.[offset + 5] <<< 40)
    ||| (uint64 bytes.[offset + 4] <<< 32)
    ||| (uint64 bytes.[offset + 3] <<< 24)
    ||| (uint64 bytes.[offset + 2] <<< 16)
    ||| (uint64 bytes.[offset + 1] <<< 8)
    ||| uint64 bytes.[offset] // LSB

[<Obsolete>]
let inline (|ReadOnlySpan|) (bytes: byte[]) = System.ReadOnlySpan<byte> bytes

[<Obsolete>]
let ofSpan length (bytes: Span<byte>) =
    let mutable bytes' = bytes.Slice(0, length).ToArray()
    Unsafe.As<_, ImmutableArray<byte>> &bytes'

let print (bytes: ReadOnlySpan<byte>) =
    let sb = StringBuilder(bytes.Length * 5 - 1)
    for i = 0 to bytes.Length - 1 do
        if i > 0 then sb.Append ' ' |> ignore
        Printf.bprintf sb "0x%02X" bytes.[i]
    sb.ToString()
