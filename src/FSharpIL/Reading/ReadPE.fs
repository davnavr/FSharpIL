[<RequireQualifiedAccess>]
module FSharpIL.Reading.ReadPE

open System
open System.Collections.Immutable
open System.IO

let fromStream<'State> stream state reader =
    invalidOp "bad"
