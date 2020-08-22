module FSharpIL.ReadPETests

open System.IO

open Fuchu

open FSharpIL.Utilities

let testPE name source body =
    test name {
        let file =
            io {
                use stream = source()
                return! ReadPE.fromStream name stream
            }
            |> IO.run
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
                    ]
                for tpe in types do
                    let assm = tpe.Assembly
                    testPE
                        (assm.GetName().Name)
                        (fun() -> File.OpenRead assm.Location)
                        ReadResult.get
            ]

        testPE
            "reading fails when magic number is incorrect"
            (fun() ->
                let data = bytes { 1; 2; 3; 4; } in new MemoryStream(data))
            (function
            | InvalidDOSHeader -> ())
    ]
    |> testList "reading tests"
