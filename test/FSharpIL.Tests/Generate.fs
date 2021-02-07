namespace FSharpIL

open FsCheck

open Expecto

[<Struct>]
type VersionString = VersionString of string

type Generate() =
    static member VersionString() =
        gen {
            let! str =
                Arb.from<string>
                |> Arb.toGen
                |> Gen.filter
                    (fun str ->
                        not(System.String.IsNullOrEmpty str) && System.Text.Encoding.UTF8.GetByteCount str <= 255)
            return VersionString str
        }
        |> Arb.fromGen

[<RequireQualifiedAccess>]
module Generate =
    let config =
        let arbs =
            [
                typeof<Generate>
            ]
        { FsCheckConfig.defaultConfig with arbitrary = arbs @ FsCheckConfig.defaultConfig.arbitrary }
