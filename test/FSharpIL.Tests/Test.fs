module FSharpIL.Test

open Expecto

[<EntryPoint>]
let main argv = runTestsInAssemblyWithCLIArgs Seq.empty [| "--sequenced" |] // TODO: Replace this temporary array with argv
