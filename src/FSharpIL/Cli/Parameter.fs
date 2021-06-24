namespace FSharpIL.Cli

open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Signatures

[<IsReadOnly; Struct>]
type Parameter =
    { //Kind: ParameterKind
      //DefaultValue: ConstantOffset voption
      ParamName: Identifier voption
      Type: EncodedType }
