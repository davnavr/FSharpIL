﻿[<RequireQualifiedAccess>]
module internal FSharpIL.Size

let PEHeader = Magic.DosStub.Length + Magic.PESignature.Length |> uint32

let [<Literal>] CoffHeader = 20

let [<Literal>] OptionalHeader = 0xE0us

/// The length of a single section header.
let [<Literal>] SectionHeader = 40
