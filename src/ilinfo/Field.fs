[<RequireQualifiedAccess>]
module ILInfo.Field

let printValue =
    function
    | value -> printf "%O" value

let printField name value =
    printf "%s = " name
    printValue value
    stdout.WriteLine()
