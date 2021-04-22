namespace FSharpIL

open Expecto

open FsCheck

open FSharpIL.Metadata
open FSharpIL.Metadata.Unchecked
open FSharpIL.PortableExecutable

type Operation =
    | AddTypeDef
    | ReferenceAssembly of System.Version * PublicKeyOrToken * AssemblyCulture

type ValidAssembly =
    { File: PEFile
      Operations: Operation[] }

[<AutoOpen>]
module private Generate =
    // TODO: Test generation of PE file with different values for alignment.
    let private alignment =
        let inline pow2 num = pown 2 num
        gen {
            let! falignment = Gen.choose(9, 14)
            let! salignment = Gen.choose(falignment + 1, 15)
            return Alignment.create (pow2 salignment) (pow2 falignment) |> Option.get
        }

    let private identifier =
        Arb.generate |> Gen.map (fun (NonEmptyString identifier) -> Identifier.ofStr identifier)

    let private modulet: Gen<ModuleTable> =
        (fun name mvid -> { Name = name; Mvid = mvid }) <!> identifier <*> Arb.generate

    let metadata: Gen<Operation[] * CliMetadata> =
        let generate mdle (operations: Operation[]) =
            let builder = CliMetadataBuilder mdle

            // TODO: Add Assembly row

            for op in operations do
                match op with
                | ReferenceAssembly(ver, pkey, culture) ->
                    ()
                | _ -> ()

            operations, CliMetadata builder
        generate <!> modulet <*> Arb.generate

    let assembly: Gen<ValidAssembly> =
        gen {
            let! (operations, md) = metadata
            let! characteristics = Arb.generate<ImageFileFlags>
            return
                { File = PEFile.ofMetadata characteristics md
                  Operations = operations }
        }

// TODO: Use model based testing with Command<_, _> and Mono.Cecil

type Generators =
    static member ValidMetadata() = Arb.fromGen metadata
    static member ValidAssembly() = Arb.fromGen assembly

[<AutoOpen>]
module Helpers =
    let private config =
        { FsCheckConfig.defaultConfig with // TODO: Instead of registering arbs here, do Arb.register in the entrypoint function.
            arbitrary = typeof<Generators> :: FsCheckConfig.defaultConfig.arbitrary }

    let testProperty name body = testPropertyWithConfig config name body
