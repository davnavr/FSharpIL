namespace FSharpIL.Utilities

type internal Process<'Result, 'Error> = unit -> Result<'Result, 'Error>

module internal Process =
    let fail err: Process<_, _> = fun() -> Error err
    let run (action: Process<_, _>) = action()

type internal ProcessBuilder() =
    member _.Bind(it, body) =
        fun() -> it() |> Result.bind (fun value -> body value ())
    member _.Delay(body): Process<_, _> = body()
    member _.Return(it) = fun() -> Ok it
    member _.ReturnFrom(it: Process<_, _>) = it
    member _.Using(resource, body): Process<_, _> =
        fun() -> using resource (fun it -> body it ())
