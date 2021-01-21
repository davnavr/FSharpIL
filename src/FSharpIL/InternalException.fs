namespace FSharpIL

open System

/// The exception that is thrown by FSharpIL when something goes wrong.
[<Sealed; Serializable>]
type internal InternalException(e: exn) =
    inherit Exception (sprintf "An exception internal to FSharpIL has occured. Consider filling an issue and providing a copy of the stack trace at %s." InternalException.IssueUrl, e)

    static member val IssueUrl = "https://github.com/davnavr/FSharpIL/issues"
