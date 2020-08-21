﻿[<RequireQualifiedAccess>]
module internal ILInfo.Utilities.Bytes

let print bytes =
    bytes
    |> Seq.map (sprintf "0x%02X")
    |> String.concat " "

type Builder() =
    member _.Combine(a: byte[], b) = Array.append a b
    member _.Delay(f: unit -> byte[]) = f()
    member _.Yield(b: byte) = Array.singleton b
    member this.Yield(c: char) = this.Yield(byte c)
    member this.Yield(i: int) = this.Yield(byte i)
    member _.Zero() = Array.empty<byte>
