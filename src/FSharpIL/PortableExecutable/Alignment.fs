namespace FSharpIL.PortableExecutable

open System.ComponentModel
open System.Runtime.CompilerServices

[<RequireQualifiedAccess>]
module Alignment =
    [<Struct; IsReadOnly; EditorBrowsable(EditorBrowsableState.Never)>]
    type Info =
        private
            { Section: uint32
              File: uint32 }

        static member Default = { Section = 0x2000u; File = 0x200u; }

        /// Always greater than the FileAlignment.
        member this.SectionAlignment = this.Section
        member this.FileAlignment = this.File // TODO: Specification says this should always be 0x200, should this be changed?

    let create salignment falignment =
        let check num =
            match num with
            | 512
            | 1024
            | 2048
            | 4096
            | 8192 
            | 16384
            | 32768 -> uint32 num |> Some
            | _ -> None
        match (check salignment, check falignment) with
        | (Some sa, Some fa) when salignment >= falignment ->
            Some { Section = sa; File = fa }
        | _ -> None

type Alignment = Alignment.Info
