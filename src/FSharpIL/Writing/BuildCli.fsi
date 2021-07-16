/// Contains types and modules for building CLI metadata modules (I.9).
module FSharpIL.Writing.BuildCli

open FSharpIL
open FSharpIL.Metadata

open FSharpIL.Cli

[<NoComparison; NoEquality>]
[<RequireQualifiedAccess>]
type ModuleUpdate =
    internal
    | AddDefinedType of DefinedType
    | Finish

[<RequireQualifiedAccess>]
module ModuleUpdate =
    val finish : ModuleUpdate

//type SomeDispatchThing =
//    member _.SendSomeUpdate<'T when 'T : struct>() = ...

[<NoComparison; NoEquality>]
type ModuleBuilder<'State> =
    { Update: 'State -> ModuleUpdate // Update: 'State -> SomeDispatchThing -> SomeReturnType
      Warning: ('State -> IValidationWarning -> 'State) option
      ReferenceType: 'State -> ReferencedType -> 'State
      DefineType: 'State -> DefinedType -> 'State }

[<RequireQualifiedAccess>]
module ModuleBuilder =
    [<GeneralizableValue>]
    val ignored<'State> : ModuleBuilder<'State>

    val run<'State> :
        header: CliHeader ->
        root: CliMetadataRoot<Omitted, Omitted> ->
        name: Identifier ->
        mvid: System.Guid ->
        builder: ModuleBuilder<'State> ->
        state: 'State -> ValidationResult<CliModuleBuilder * 'State>
