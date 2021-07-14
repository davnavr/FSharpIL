﻿module FSharpIL.Writing.BuildCli

open System
open System.Collections.Generic

open FSharpIL.Metadata

open FSharpIL.Cli

[<NoComparison; NoEquality>]
type ModuleUpdate =
    | AddDefinedType of DefinedType
    | Finish

[<RequireQualifiedAccess>]
module ModuleUpdate =
    let finish = ModuleUpdate.Finish

[<NoComparison; NoEquality>]
type ModuleBuilder<'State> =
    { Update: 'State -> ModuleUpdate
      Warning: ('State -> IValidationWarning -> 'State) option
      ReferenceType: 'State -> ReferencedType -> 'State
      DefineType: 'State -> DefinedType -> 'State }

[<RequireQualifiedAccess>]
module ModuleBuilder =
    type CachedBuilder<'State> private () =
        static member val Ignored: ModuleBuilder<'State> =
            let inline ignore1 state _ = state
            { Update = fun _ -> Finish
              Warning = None
              ReferenceType = ignore1
              DefineType = ignore1 }

    let ignored<'State> = CachedBuilder<'State>.Ignored

    let run header root name mvid builder state =
        let builder' =
            CliModuleBuilder (
                name = name,
                mvid = mvid,
                ?assembly = failwith "TODO: How to get assembly?",
                header = header,
                root = root,
                ?warnings = Option.map (fun _ -> LinkedList<_>() :> ICollection<_>) builder.Warning
           )

        let rec inner state =
            match builder.Update state with
            | ModuleUpdate.Finish -> Ok(builder', state)

        inner state
