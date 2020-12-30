[<AutoOpen>]
module FSharpIL.InternalException

open System

let private helpLink = "https://github.com/davnavr/FSharpIL/issues"

/// The exception that is thrown by FSharpIL when something goes wrong.
[<Sealed; Serializable>]
type InternalException internal (msg: string) =
    inherit Exception (sprintf "An exception internal to FSharpIL has occured. Consider filing an issue at %s. %s" helpLink msg)

/// <summary>
/// Adds additional information to an <see cref="FSharpIL.InternalException.InternalException"/> before it is raised.
/// </summary>
let internalExn data msg =
    let exn = InternalException msg
    exn.HelpLink <- helpLink
    for (key: string, value: obj) in data do
        exn.Data.Item <- key :> obj, value
    raise exn
