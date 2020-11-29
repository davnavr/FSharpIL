[<RequireQualifiedAccess>]
module internal FSharpIL.Utilities.Bytes
// Little endian

let inline private andb value by =
    byte (value &&& by)

let inline empty amt = Array.replicate amt 0uy

let inline ofU16 value =
    let value' = uint16 value
    [| andb value' 0xFFus; andb (value' >>> 8) 0xFFus |]

let inline ofU32 value =
    let value' = uint32 value
    [|
        andb value' 0xFFu
        andb (value' >>> 8) 0xFFu
        andb (value' >>> 16) 0xFFu
        andb (value' >>> 24) 0xFFu
    |]
