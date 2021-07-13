namespace FSharpIL.Writing

open System
open System.Collections.Generic

open FSharpIL.Cli
open FSharpIL.Metadata
open FSharpIL.Metadata.Cil

open FSharpIL.Writing.Cil

open FSharpIL.Utilities.Collections

[<NoComparison; NoEquality>]
type UpdateModule =
    | AddDefinedType of DefinedType
    | Finish

[<NoComparison; NoEquality>]
type ModuleBuilder<'State> =
    { ReferenceType: 'State -> ReferencedType -> 'State
      DefineType: 'State -> DefinedType -> 'State }

[<RequireQualifiedAccess>]
module ModuleBuilder =
    type CachedBuilder<'State> private () =
        static member val Ignored: ModuleBuilder<'State> =
            let inline ignore1 state _ = state
            { ReferenceType = ignore1
              DefineType = ignore1 }

    let ignored<'State> = CachedBuilder<'State>.Ignored
