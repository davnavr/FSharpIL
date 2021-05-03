[<RequireQualifiedAccess>]
module internal FSharpIL.Convert

let inline (|U4|) num = uint32 num
let inline (|U8|) num = uint64 num
