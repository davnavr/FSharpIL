[<RequireQualifiedAccess>]
module internal FSharpIL.Size

let PEHeader = Magic.DosStub.Length + Magic.PESignature.Length |> uint32

let [<Literal>] CoffHeader = 20

let [<Literal>] OptionalHeader = 0xE0us

/// The length of a single section header.
let [<Literal>] SectionHeader = 40

/// The length of the CLI header, in bytes.
let [<Literal>] CliHeader = 0x48u

/// The size of a fat method body header, as a number of 4-byte integers (II.25.4.3).
let [<Literal>] FatFormat = 3us
