module FSharpIL.WritePETests

open System.IO
open System.Reflection.PortableExecutable

open Expecto

open FSharpIL.PortableExecutable

[<Tests>]
let tests =
    testList "write PE" [
        testCase "PE is valid" <| fun() ->
            let data =
                use str = new MemoryStream()
                do WritePE.toStream str PEFile.Default
                str.GetBuffer()
            use source = new MemoryStream(data, false)
            use reader = new PEReader(source, PEStreamOptions.PrefetchEntireImage)
            Expect.isNotNull reader.PEHeaders "PE Headers should be generated correctly"
            Expect.isTrue reader.HasMetadata "Generated PE should contain metadata"
    ]
