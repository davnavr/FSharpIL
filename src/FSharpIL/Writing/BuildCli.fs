module FSharpIL.Writing.BuildCli

open System.Collections.Generic

open FSharpIL.Metadata

open FSharpIL.Cli

[<NoComparison; NoEquality>]
type ModuleUpdate =
    | AddDefinedType of TypeDefinition
    | Finish

[<RequireQualifiedAccess>]
module ModuleUpdate =
    let finish = ModuleUpdate.Finish

[<NoComparison; NoEquality>]
type ModuleBuilder<'State> =
    { Update: 'State -> ModuleUpdate
      Warning: ('State -> IValidationWarning -> 'State) option
      ReferenceType: 'State -> TypeReference -> 'State
      DefineType: 'State -> TypeDefinition -> 'State }

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
            let warnings =
                match builder.Warning with
                | Some _ -> Some(LinkedList<_>() :> ICollection<_>)
                | None -> None

            CliModuleBuilder (
                name = name,
                mvid = mvid,
                cliMetadataHeader = header,
                cliMetadataRoot = root,
                ?warnings = warnings
            )

        let rec inner state =
            match builder.Update state with
            | ModuleUpdate.Finish -> ValidationResult.Ok(builder', state)

        inner state
