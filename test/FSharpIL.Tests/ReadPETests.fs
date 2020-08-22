module FSharpIL.ReadPETests

open System.IO

open Fuchu

open FSharpIL.Utilities

let tests =
    [
        test "correct assembly" {
            let assembly =
                io {
                    let path = typeof<obj>.Assembly.Location
                    use a = null;
                    return! ReadPE.fromPath path
                }
                |> IO.run
            match assembly with
            | ValidFile _ -> ()
            | err -> string err |> failwith // TODO: Find better way to assert the result.
        }

        test "reading fails when magic number is incorrect" {
            let file =
                io {
                    let data = bytes { 1; 2; 3; 4; }
                    use source = new MemoryStream(data)
                    return! ReadPE.fromStream "test" source
                }
                |> IO.run
            match file with
            | InvalidDOSHeader(value, at) ->
                Assert.Equal("bad byte", (value, at), (1uy, 0uy))
            | ValidFile _ -> failwith "unexpected success"
            | err -> string err |> failwith
        }
    ]
    |> testList "reading tests"
