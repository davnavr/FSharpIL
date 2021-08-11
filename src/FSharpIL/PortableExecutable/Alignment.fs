namespace FSharpIL.PortableExecutable

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type Alignment =
    private { Section: uint16; File: uint16 }
    static member Default = { Section = 0x2000us; File = 0x200us; }
    /// Always greater than the FileAlignment.
    member this.SectionAlignment = uint32 this.Section
    // TODO: Specification says this should always be 0x200, should this be a fixed value?
    member this.FileAlignment = uint32 this.File

[<RequireQualifiedAccess>]
module Alignment =
    let private (|Valid|_|) alignment =
        match alignment with
        | 512u
        | 1024u
        | 2048u
        | 4096u
        | 8192u
        | 16384u
        | 32768u -> uint16 alignment |> Some
        | _ -> None

    let tryCreate salignment falignment =
        match (salignment, falignment) with
        | Valid sa, Valid fa when salignment >= falignment -> ValueSome { Section = sa; File = fa }
        | _ -> ValueNone
