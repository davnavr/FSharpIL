[<RequireQualifiedAccess>]
module FSharpIL.Writing.BuildCli

open FSharpIL
open FSharpIL.Metadata

val internal metadata<'State> :
    CliHeader ->
    CliMetadataRoot<Omitted, Omitted> ->
    name: Identifier ->
    mvid: System.Guid ->
    ModuleBuilder<'State> ->
    'State ->
    CliMetadataBuilder * 'State
