namespace FSharpIL.Writing

open FSharpIL.Cli

[<NoComparison; NoEquality>]
type ModuleBuilderCommand =
    internal
    | AddDefinedType of DefinedType

[<NoComparison; NoEquality>]
type ModuleBuilder<'State> =
    { ReferenceType: 'State -> ReferencedType -> 'State
      DefineType: 'State -> DefinedType -> 'State }

[<RequireQualifiedAccess>]
module ModuleBuilder =
    [<GeneralizableValue>]
    val ignored<'State> : ModuleBuilder<'State>
