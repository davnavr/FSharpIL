namespace FSharpIL.Metadata

// TODO: IMPORTANT, make builder CE return a (MetadataBuilderState -> 'T) instead of a BuilderExpression<_>

[<Sealed>]
type CliMetadataBuilder___New internal () =

    member inline _.BindCommon(expr: _ -> _, body: _ -> _ -> _) =
        fun (state: MetadataBuilderState) ->
            let result = expr state
            body result state

    member inline _.Zero() = ()

[<Sealed>]
type CliMetadataBuilder internal () =
    member inline _.Bind(expr: BuilderExpression<'T>, body: 'T -> BuilderExpression<_>): BuilderExpression<_> =
        fun state ->
            expr state |> Result.bind (fun result -> body result state)

    member inline _.Bind(expr: unit -> BuilderResult<'T>, body: 'T -> BuilderExpression<_>): BuilderExpression<_> =
        fun state ->
            expr() |> Result.bind (fun result -> body result state)

    member inline _.Bind(expr: _ -> unit, body: _ -> BuilderExpression<_>): BuilderExpression<_> =
        fun state -> expr state; body () state

    member inline _.BindCommon(expr: MetadataBuilderState -> 'T, body: _ -> BuilderExpression<_>): BuilderExpression<_> =
        fun state ->
            let result = expr state in body result state

    member inline this.Bind(expr: _ -> SimpleIndex<_>, body) = this.BindCommon(expr, body)
    member inline this.Bind(expr: _ -> TaggedIndex<_, _>, body) = this.BindCommon(expr, body)
    member inline this.Bind(expr: _ -> TypeBuilder<_, _, _, _>, body) = this.BindCommon(expr, body)

    member inline _.Return result: BuilderExpression<_> = fun _ -> Ok result

    member inline this.Zero() = this.Return()
