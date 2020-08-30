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
    
        static member Default = { Section = 512u; File = 512u; }
    
        member this.SectionAlignment = this.Section
        member this.FileAlignment = this.File

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
