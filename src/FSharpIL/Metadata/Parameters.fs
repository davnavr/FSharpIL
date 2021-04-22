/// Contains functions for adding parameters to methods.
[<RequireQualifiedAccess>]
module FSharpIL.Metadata.Parameters

open System.Collections.Immutable
open System.Runtime.CompilerServices

let tryAdd (builder: CliMetadataBuilder) method (parameters: ImmutableArray<Parameter>) =
    let mutable parameters' = Array.zeroCreate<ParamRow> parameters.Length
    let mutable constanti = 0
    let constants = Array.zeroCreate<ConstantBlob> parameters.Length
    for i = 0 to parameters.Length - 1 do
        let param = &parameters.ItemRef i
        match param.Value with
        | ValueSome value ->
            constants.[constanti] <- value
            constanti <- constanti + 1
        | ValueNone -> ()
        parameters'.[i] <- ParamRow(param.Flags, uint16 i + 1us, param.Name)
    let result = ParamRows.add (Unsafe.As &parameters') builder method
    match result with
    | Ok indices when constanti > 0 ->
        while constanti > 0 do
            constanti <- constanti - 1
            if builder.Constant.TryAdd(indices.[constanti], constants.[constanti]).IsNone then
                sprintf "Unable to add default value to parameter %A" indices.[constanti] |> invalidOp
        Ok indices
    | _ -> result

let inline add builder method parameters = tryAdd builder method parameters |> ValidationError.check

let tryNamed builder method names = ParamRows.addNamed names builder method

let inline named builder method names = tryNamed builder method names |> ValidationError.check

let inline trySingleton builder method (parameter: Parameter) = tryAdd builder method (ImmutableArray.Create parameter)

let inline singleton builder method parameter = trySingleton builder method parameter |> ValidationError.check
