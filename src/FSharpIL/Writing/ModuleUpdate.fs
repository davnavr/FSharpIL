namespace FSharpIL.Writing

open FSharpIL.Metadata
open FSharpIL.Metadata.Tables

open FSharpIL.Cli

[<NoComparison; NoEquality>]
[<RequireQualifiedAccess>]
type ModuleUpdate =
    internal
    | DefineAssembly of DefinedAssembly
    | DefineType of DefinedType
    | Finish

