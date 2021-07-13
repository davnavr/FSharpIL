namespace FSharpIL.Writing

open FSharpIL.Metadata.Tables

open FSharpIL.Cli

[<NoComparison; NoEquality>]
type UpdateModule =
    internal
    | AddDefinedType of DefinedType
    | Finish

[<RequireQualifiedAccess>]
module UpdateModule =
    val finish : UpdateModule

    // TODO: Have module with functions for defining specific types e.g. addDefinedInterface, addDefinedEnum, etc. Maybe make a "DefineType" module.
    val addDefinedType : definition: DefinedType -> UpdateModule // TODO: Maybe just search the tree and add any missing types (parent, extends)

[<NoComparison; NoEquality>]
type ModuleBuilder<'State> =
    { ReferenceType: 'State -> ReferencedType -> 'State
      DefineType: 'State -> DefinedType -> 'State }

[<RequireQualifiedAccess>]
module ModuleBuilder =
    [<GeneralizableValue>]
    val ignored<'State> : ModuleBuilder<'State>
