namespace ILInfo.Utilities

type internal IO<'Result> = unit -> 'Result

type internal IOBuilder() =
    member _.Combine(i1, i2) = fun() -> i1(), i2()
    member _.Combine(u: IO<unit>, i) = fun() -> u(); i()
    member _.Delay(body) = fun() -> body()()
    member _.Return(it) = fun() -> it
    member _.Run(action) = action()
    member _.TryFinally(body, fin) =
        fun() -> try body() finally fin()
    member _.Using(resource, body) =
        using resource (fun it -> fun() -> body it)
    member _.Zero() = fun() -> ()
