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
            let! kind = Arb.generate<IsDll>

            let! cli =
                gen {
                    let! mdle = moduleTable
                    let! state = Gen.fresh (fun() -> MetadataBuilderState mdle)

                    let! assembly =
                        gen {
                            let! (NonEmptyString name) =
                                Gen.filter
                                    (fun (NonEmptyString str) -> str.IndexOfAny [| ':'; '\\'; '/' |] = -1)
                                    Arb.generate
                            let! version =
                                Gen.choose (0, 9)
                                |> Gen.four
                                |> Gen.map Version
                            let! publicKey = Arb.generate
                            let! culture = Arb.generate
                            let assembly =
                                { Name = AssemblyName.ofStr name
                                  HashAlgId = ()
                                  Version = version
                                  Flags = ()
                                  PublicKey = publicKey
                                  Culture = culture }
                            return CliMetadata.setAssembly assembly state
                        }
                        |> Gen.optionOf

                    let! typeRef = Gen.choose (-1, 0xFF)
                    for _ = 0 to typeRef do
                        ()

                    let! typeDef = Gen.choose (-1, 0xFF)
                    for _ = 0 to typeDef do
                        let! visibility =
                            [|
                                Gen.constant TypeVisibility.NotPublic
                                Gen.constant TypeVisibility.Public

                                // TODO: Add support for nested visibility.
                            |]
                            |> Gen.oneof
                        let! flags = Arb.generate<ClassFlags> |> Gen.map staticClassFlags
                        let! name = identifier
                        let! (NonEmptyString ns) = Arb.generate
                        let! extends =
                            [|
                                Gen.constant Extends.Null

                                // TODO: Add support for other extends.
                            |]
                            |> Gen.oneof

                        // TODO: Add support for other classes.
                        CliMetadata.addStaticClass
                            { Access = visibility
                              Flags = flags
                              ClassName = name
                              TypeNamespace = ns
                              Extends = extends
                              Fields = MemberList.Empty
                              Methods = MemberList.Empty }
                            state
                        |> ignore

                    return CliMetadata state
                }

            return PEFile.ofMetadata kind cli |> ValidAssembly
        }
        |> Arb.fromGen

let private config =
    let arbs =
        [
            typeof<Generate>
        ]
    { FsCheckConfig.defaultConfig with arbitrary = arbs @ FsCheckConfig.defaultConfig.arbitrary }

let testProperty name body = testPropertyWithConfig config name body
