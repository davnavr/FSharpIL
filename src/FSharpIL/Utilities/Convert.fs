[<RequireQualifiedAccess>]
module internal FSharpIL.Utilities.Convert

let inline (|I4|) num = int32 num
let inline (|U1|) num = uint8 num
let inline (|U2|) num = uint16 num
let inline (|U4|) num = uint32 num
let inline (|U8|) num = uint64 num
