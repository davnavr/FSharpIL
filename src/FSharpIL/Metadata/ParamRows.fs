[<RequireQualifiedAccess>]
module internal FSharpIL.Metadata.ParamRows

open System.Collections.Immutable
open System.Runtime.CompilerServices

let named (names: string[]) =
    match names with
    | null
    | [||] -> ImmutableArray.Empty
    | _ ->
        let mutable parameters = Array.zeroCreate<ParamRow> names.Length
        for i = 0 to names.Length - 1 do
            parameters.[i] <- ParamRow(ParamFlags(), uint16 i + 1us, names.[i])
        Unsafe.As<_, ImmutableArray<ParamRow>> &parameters

let add rows (builder: CliMetadataBuilder) method =
    match builder.Param.TryAdd(method, rows) with
    | ValueSome parameters -> Ok parameters
    | ValueNone -> DuplicateParamError(method).ToResult()

let addNamed names = named names |> add
