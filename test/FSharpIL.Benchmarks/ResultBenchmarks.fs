namespace FSharpIL

open BenchmarkDotNet.Attributes

[<RequireQualifiedAccess>]
module ResultBenchmarks =
    let okValueTuple (chunk: inref<ChunkedMemory>) =
        chunk.IsValidOffset 0u |> ignore
        Ok(struct(chunk, "result"))
    let errorValueTuple (chunk: inref<ChunkedMemory>) =
        chunk.IsValidOffset 0u |> ignore
        Error(obj())
    let okOutrefChunk (chunk: outref<ChunkedMemory>) =
        chunk.IsValidOffset 0u |> ignore
        Ok "result"
    let errorOutrefChunk(chunk: outref<ChunkedMemory>) =
        chunk.IsValidOffset 0u |> ignore
        Error(obj())
    let okOptionalResult (chunk: outref<ChunkedMemory>) (result: outref<string>) =
        chunk.IsValidOffset 0u |> ignore
        result <- "result"
        None
    let errorOptionalResult (chunk: outref<ChunkedMemory>) (_: outref<string>) =
        chunk.IsValidOffset 0u |> ignore
        Some(obj())

[<StatisticalTestColumn>]
[<MemoryDiagnoser>]
type ResultBenchmarks () =
    let chunk = ChunkedMemory.empty

    [<Benchmark>]
    member _.ValueTupleOk(): Result<_, obj> =
        match ResultBenchmarks.okValueTuple &chunk with
        | Ok(_, str) -> Ok str.Length
        | Error err -> Error err
    [<Benchmark>]
    member _.ValueTupleError() =
        match ResultBenchmarks.errorValueTuple &chunk with
        | Ok(_, str: string) -> Ok str.Length
        | Error err -> Error err

    [<Benchmark>]
    member _.OutrefChunkOk(): Result<_, obj> =
        let mutable chunk' = chunk
        match ResultBenchmarks.okOutrefChunk &chunk' with
        | Ok str -> Ok str.Length
        | Error err -> Error err
    [<Benchmark>]
    member _.OutrefChunkError() =
        let mutable chunk' = chunk
        match ResultBenchmarks.errorOutrefChunk &chunk' with
        | Ok(str: string) -> Ok str.Length
        | Error err -> Error err

    [<Benchmark>]
    member _.OptionalResultOk(): Result<_, obj> =
        let mutable chunk', result = chunk, null
        match ResultBenchmarks.okOptionalResult &chunk' &result with
        | None -> Ok result.Length
        | Some err -> Error err
    [<Benchmark>]
    member _.OptionalResultError() =
        let mutable chunk', result = chunk, null
        match ResultBenchmarks.errorOptionalResult &chunk' &result with
        | None -> Ok result.Length
        | Some err -> Error err
