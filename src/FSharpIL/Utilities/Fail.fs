[<AutoOpen>]
module FSharpIL.Utilities.Fail

open System

let inline noImpl msg = raise(NotImplementedException msg)
let inline notSupported msg = raise(NotSupportedException msg)
let inline argOutOfRange argumentName argument message = raise(ArgumentOutOfRangeException(argumentName, argument, message))
