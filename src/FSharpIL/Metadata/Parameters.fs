/// Contains functions for adding parameters to methods.
[<RequireQualifiedAccess>]
module FSharpIL.Metadata.Parameters

open System.Collections.Immutable
open System.Runtime.CompilerServices

let tryAdd (builder: CliMetadataBuilder) method (parameters: ImmutableArray<Parameter>) =
    let mutable parameters' = Array.zeroCreate<ParamRow> parameters.Length
    for i = 0 to parameters.Length - 1 do
        let param = &parameters.ItemRef i

        match param.Value with
        | ValueSome value ->  invalidOp "Default parameter values are currently not supported"
        | ValueNone -> ()

        parameters'.[i] <- ParamRow(param.Flags, uint16 i + 1us, param.Name)
    ParamRows.add (Unsafe.As &parameters') builder method

let inline add builder method parameters = tryAdd builder method parameters |> ValidationError.check

let tryNamed builder method names = ParamRows.addNamed names builder method

let inline named builder method names = tryNamed builder method names |> ValidationError.check

let inline trySingleton builder method (parameter: Parameter) = tryAdd builder method (ImmutableArray.Create parameter)

let inline singleton builder method parameter = trySingleton builder method parameter |> ValidationError.check
