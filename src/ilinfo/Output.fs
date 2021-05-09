[<RequireQualifiedAccess>]
module ILInfo.Output

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type Format =
    | IL
    | Html

let write format =
    match format with
    | IL -> ILOutput.write
    | Html -> invalidOp "HTML output is not yet supported"
