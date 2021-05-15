﻿[<RequireQualifiedAccess>]
module internal FSharpIL.Convert

let inline (|I4|) num = int32 num
let inline (|U4|) num = uint32 num
let inline (|U8|) num = uint64 num
