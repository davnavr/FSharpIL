[<AutoOpen>]
module FSharpIL.Utilities.Collections.CollectionFail

open FSharpIL.Utilities

let enumeratorNotStarted() = invalidOp "Cannot retrieve current item, the enumerator must first be started by calling MoveNext"
let enumeratorReachedEnd() = invalidOp "Cannot retrieve current item, the end of the collection has been reached"
let enumeratorCannotReset() = noImpl "Cannot reset enumerator"
