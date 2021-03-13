module FSharpIL.Generate

open FsCheck

open Expecto

open System
open System.Text

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

[<Struct>]
type ValidAssembly = ValidAssembly of PEFile

[<AutoOpen>]
module private Helpers =
    let identifier =
        gen {
            let! (NonEmptyString identifier) = Arb.generate
            return Identifier.ofStr identifier
        }

    let moduleTable =
        gen {
            let! name = identifier
            let! mvid = Arb.generate
            return
                { Name = name
                  Mvid = mvid }
        }

type Generate() =
    static member ValidAssembly() =
        gen {
            return invalidOp "TODO: Figure out how to generate things" |> ValidAssembly
        }
        |> Arb.fromGen

let private config =
    let arbs =
        [
            typeof<Generate>
        ]
    { FsCheckConfig.defaultConfig with arbitrary = arbs @ FsCheckConfig.defaultConfig.arbitrary }

let testProperty name body = testPropertyWithConfig config name body
