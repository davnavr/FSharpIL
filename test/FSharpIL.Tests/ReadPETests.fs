﻿module FSharpIL.ReadPETests

open System.IO

open Fuchu

open FSharpIL.Utilities

let testPE name source body =
    test name {
        let file =
            proc {
                use stream = source()
                return! ReadPE.fromStream name stream
            }
            |> Process.run
        body file |> ignore
    }

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
            (fun() ->
                let data = bytes { 1; 2; 3; 4; } in new MemoryStream(data))
            (function
            | Error(IncorrectDOSMagic(1uy, 2uy)) -> ())
    ]
    |> testList "reading tests"
