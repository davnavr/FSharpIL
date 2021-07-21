namespace FSharpIL.Cli

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata

[<IsReadOnly; Struct; NoComparison; StructuralEquality; RequireQualifiedAccess>]
type ParameterKind =
    | Default
    | InRef
    | OutRef

    interface IEquatable<ParameterKind>

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type ParameterType =
    | T of CliType
    | ByRef of modifiers: ImmutableArray<ModifierType> * CliType
    | TypedByRef of modifiers: ImmutableArray<ModifierType>

    interface IEquatable<ParameterType>

[<RequireQualifiedAccess>]
module ParameterType =
    val TypedByRef' : ParameterType

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type Parameter =
    { Kind: ParameterKind
      DefaultValue: Constant voption
      ParamName: Identifier voption } // TODO: Have field that allows setting of Optional flag.

type ParameterList = int32 -> ParameterType -> Parameter

[<RequireQualifiedAccess>]
module Parameter =
    val flags: parameter: inref<Parameter> -> FSharpIL.Metadata.Tables.ParamFlags

    val named: name: Identifier -> Parameter

    val emptyList: ParameterList

// TODO: Allow more efficient ways of generating parameter list
(*
type IParameterList = interface
    abstract GetParameter: index: int32 * parameterType: inref<ParamItem>
end

type ParameterList = delegate of int32 * inref<ParamItem> -> Parameter voption
*)
