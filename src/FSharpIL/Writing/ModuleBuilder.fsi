﻿namespace FSharpIL.Writing

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

    // TODO: Have module with functions for defining specific types e.g. addDefinedInterface, addDefinedEnum, etc. Maybe make a "DefineType" module.
    //val addDefinedType : definition: (SomeParentRetrievalType -> SomeExtendsRetrievalType -> DefinedType) -> ModuleBuilderCommand
    val addDefinedType : definition: DefinedType -> ModuleBuilderCommand // TODO: Maybe just search the tree and add any missing types (parent, extends)

[<NoComparison; NoEquality>]
type ModuleBuilder<'State> =
    { ReferenceType: 'State -> ReferencedType -> 'State
      DefineType: 'State -> DefinedType -> 'State }

[<RequireQualifiedAccess>]
module ModuleBuilder =
    [<GeneralizableValue>]
    val ignored<'State> : ModuleBuilder<'State>
