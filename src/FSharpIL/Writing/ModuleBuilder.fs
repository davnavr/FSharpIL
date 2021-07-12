namespace FSharpIL.Writing

open FSharpIL.Cli

[<NoComparison; NoEquality>]
type ModuleBuilder<'State> =
    { Update: 'State -> ModuleUpdate
      Warning: ('State -> FSharpIL.Metadata.IValidationWarning -> 'State) option
      ReferenceType: 'State -> ReferencedType -> 'State
      DefineType: 'State -> DefinedType -> 'State }

[<RequireQualifiedAccess>]
module ModuleBuilder =
    type CachedBuilder<'State> private () =
        static member val Ignored: ModuleBuilder<'State> =
            let inline ignore1 state _ = state
            { Update = fun _ -> ModuleUpdate.Finish
              Warning = None
              ReferenceType = ignore1
              DefineType = ignore1 }

    let ignored<'State> = CachedBuilder<'State>.Ignored
