namespace FSharpIL.Writing

open FSharpIL.Cli

[<NoComparison; NoEquality>]
type ModuleBuilder<'State> =
    { /// The function used to update the contents of the CLI module.
      Update: 'State -> ModuleUpdate
      /// An optional function to update the state given a validation warning. If omitted, all warnings are ignored.
      Warning: ('State -> FSharpIL.Metadata.IValidationWarning -> 'State) option
      ReferenceType: 'State -> ReferencedType -> 'State
      DefineType: 'State -> DefinedType -> 'State }

[<RequireQualifiedAccess>]
module ModuleBuilder =
    [<GeneralizableValue>]
    val ignored<'State> : ModuleBuilder<'State>
