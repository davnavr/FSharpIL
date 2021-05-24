namespace FSharpIL

open System

/// The exception that is thrown by FSharpIL when something goes wrong.
[<Sealed; Serializable; Obsolete>]
type internal InternalException (e: exn) =
    inherit Exception (InternalException.Message, e)

    static member val Message =
        sprintf
            "An exception internal to FSharpIL has occured. %s"
            "Consider filling an issue and providing a copy of the stack trace at https://github.com/davnavr/FSharpIL/issues"
