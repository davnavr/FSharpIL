namespace FSharpIL.Metadata

[<Sealed>]
type CliMetadataBuilder internal () =
    member inline _.Bind(expr: _ -> Result<'T, ValidationError>, body: 'T -> _ -> _) =
        fun (state: MetadataBuilderState) ->
            expr state |> Result.bind (fun result -> body result state)

    member inline _.Bind(expr: Result<'T, ValidationError>, body: 'T -> _ -> _) =
        fun (state: MetadataBuilderState) ->
            Result.bind (fun result -> body result state) expr

    member inline _.Bind(expr: _ -> unit, body: _ -> _ -> Result<_, ValidationError>) =
        fun (state: MetadataBuilderState) -> expr state; body () state

    member inline _.BindCommon(expr: _ -> 'T, body: 'T -> _ -> Result<_, ValidationError>) =
        fun (state: MetadataBuilderState) ->
            let result = expr state in body result state

    member inline this.Bind(expr: _ -> SimpleIndex<_>, body) = this.BindCommon(expr, body)
    member inline this.Bind(expr: _ -> TaggedIndex<_, _>, body) = this.BindCommon(expr, body)

    member inline _.Return result = fun (_: MetadataBuilderState) -> Result<_, ValidationError>.Ok result

    member inline this.Zero() = this.Return()
