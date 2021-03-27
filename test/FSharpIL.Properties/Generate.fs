module FSharpIL.Generate

open FsCheck

open FSharpIL.Metadata
open FSharpIL.PortableExecutable

type ValidAssembly = ValidAssembly of PEFile

type Operation =
    | AddTypeDef

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

let private metadata: Gen<CliMetadata> =
    let generate mdle (count: uint32) =
        let builder = CliMetadataBuilder mdle



        CliMetadata builder
    generate <!> modulet <*> Arb.generate

let private assembly: Gen<ValidAssembly> =
    gen {
        let! md = metadata
        let! characteristics = Arb.generate<ImageFileFlags>
        return PEFile.ofMetadata characteristics md |> ValidAssembly
    }

// TODO: Use model based testing with Command<_, _> and Mono.Cecil

type Generators =
    static member ValidMetadata() = Arb.fromGen metadata
    static member ValidAssembly() = Arb.fromGen assembly
