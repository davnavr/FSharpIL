namespace FSharpIL.PortableExecutable

open System.ComponentModel
open System.Runtime.CompilerServices

[<Struct; IsReadOnly; EditorBrowsable(EditorBrowsableState.Never)>]
type Alignment =
    private { Section: uint16; File: uint16 }
    static member Default = { Section = 0x2000us; File = 0x200us; }
    /// Always greater than the FileAlignment.
    member this.SectionAlignment = this.Section
    // TODO: Specification says this should always be 0x200, should this be a fixed value?
    member this.FileAlignment = this.File

[<RequireQualifiedAccess>]
module Alignment =
    let private (|Valid|_|) alignment =
        match alignment with
        | 512
        | 1024
        | 2048
        | 4096
        | 8192
        | 16384
        | 32768 -> uint16 alignment |> Some
        | _ -> None

    let create salignment falignment =
        match (salignment, falignment) with
        | Valid sa, Valid fa when salignment >= falignment ->
            Some { Section = sa; File = fa }
        | _ -> None
