namespace FSharpIL.Utilities

type internal IO<'Result> = unit -> 'Result

module internal IO =
    let run (action: IO<_>) = action()

type internal IOBuilder() =
    member _.Combine(i1, i2) = fun() -> i1(), i2()
    member _.Combine(u: IO<unit>, i) = fun() -> u(); i()
    member _.Bind(it, map: _ -> IO<_>) = it() |> map
    member _.Delay(body: IO<IO<_>>) = body()
    member _.Return(it) = fun() -> it
    member _.ReturnFrom(it: IO<_>) = it
    member _.TryFinally(body, fin) =
        fun() -> try body() finally fin()
    member _.Using(resource, body): IO<_> =
        using resource (fun it -> body it)
    member _.Zero() = fun() -> ()
