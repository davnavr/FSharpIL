module FSharpIL.ReadPETests

open System.IO

open Fuchu

open FSharpIL.Utilities

let testPE name source body =
    test name {
        ReadPE.fromStream name (source()) ()
        |> body
        |> ignore
    }

let private assertEqual exp act =
    Assert.Equal("not equal", exp, act);

let inline srcbytes arr () = new MemoryStream(Array.map byte arr)

let tests =
    [
        testList
            "valid assembly is read correctly"
            [
                let types =
                    [
                        typeof<obj>
                        typeof<System.Collections.Immutable.IImmutableList<_>>
                        typeof<Assert>
                    ]
                for tpe in types do
                    let assm = tpe.Assembly
                    testPE
                        (assm.GetName().Name)
                        (fun() -> File.OpenRead assm.Location)
                        Result.get
            ]

        testPE
            "reading fails when magic number is incorrect"
            (srcbytes [| 1; 2; 3; 4; |])
            (Error(IncorrectDOSMagic(1uy, 2uy)) |> assertEqual)

        testPE
            "reading fails when DOS stub is too short"
            (srcbytes [| 0x4D; 0x5A |])
            (Error(InvalidPESignatureOffset None) |> assertEqual)
    ]
    |> testList "reading tests"
