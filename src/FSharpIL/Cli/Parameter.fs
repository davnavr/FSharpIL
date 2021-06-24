namespace FSharpIL.Cli

open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures

[<IsReadOnly; Struct>]
[<RequireQualifiedAccess>]
type ParameterKind =
    | Default
    | InRef
    | OutRef

[<IsReadOnly; Struct>]
type Parameter =
    { Kind: ParameterKind
      DefaultValue: Constant voption
      ParamName: Identifier voption }

type ParameterList = int32 -> ParamItem -> Parameter voption

// TODO: Allow more efficient ways of generating parameter list
(*
type IParameterList = interface
    abstract GetParameter: index: int32 * parameterType: inref<ParamItem>
end

type ParameterList = delegate of int32 * inref<ParamItem> -> Parameter voption
*)
