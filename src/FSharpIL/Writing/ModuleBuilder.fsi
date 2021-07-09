namespace FSharpIL.Writing

open FSharpIL.Metadata.Tables

open FSharpIL.Cli

[<NoComparison; NoEquality>]
type ModuleBuilderCommand =
    internal
    | AddDefinedType of DefinedType
    | Finish

[<RequireQualifiedAccess>]
module ModuleBuilderSomething =
    val finish : ModuleBuilderCommand

    //val addDefinedType : definition: (SomeParentRetrievalType -> SomeExtendsRetrievalType -> DefinedType) -> ModuleBuilderCommand
    val addDefinedType : definition: TypeDefinition<'Kind> -> ModuleBuilderCommand // TODO: Maybe just search the tree and add any missing types (parent, extends)

[<NoComparison; NoEquality>]
type ModuleBuilder<'State> =
    { ReferenceType: 'State -> ReferencedType -> 'State
      DefineType: 'State -> DefinedType -> 'State }

[<RequireQualifiedAccess>]
module ModuleBuilder =
    [<GeneralizableValue>]
    val ignored<'State> : ModuleBuilder<'State>
