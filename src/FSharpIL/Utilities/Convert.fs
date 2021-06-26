[<RequireQualifiedAccess>]
module internal FSharpIL.Utilities.Convert

open System.Runtime.CompilerServices

let inline (|I4|) num = int32 num
let inline (|U1|) num = uint8 num
let inline (|U2|) num = uint16 num
let inline (|U4|) num = uint32 num
let inline (|U8|) num = uint64 num

let inline unsafeTo<'From, 'To>(source: 'From) = Unsafe.As<'From, 'To>(&Unsafe.AsRef &source)

let inline unsafeValueOption<'From, 'To when 'From : not struct and 'To : not struct>(source: 'From voption) =
    match source with
    | ValueSome o -> ValueSome(Unsafe.As<'To> o)
    | ValueNone -> ValueNone
