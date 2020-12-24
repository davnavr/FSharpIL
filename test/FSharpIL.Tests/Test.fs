module FSharpIL.Test

open Expecto

[<EntryPoint>]
let main argv = runTestsInAssemblyWithCLIArgs Seq.empty [| "--sequenced" |] // Temporary, replace with argv
