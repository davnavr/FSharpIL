namespace FSharpIL.Metadata

open System.Collections.Immutable

[<Sealed>]
type StandAloneSigTable internal
    (
        locals: ImmutableArray<Blob<MethodLocalVariables>>,
        methods: ImmutableArray<FunctionPointer>,
        locals': BlobLookup<MethodLocalVariables>
    ) =
    member _.LocalVariables = locals
    member _.MethodSignatures = methods

    member _.TotalCount = locals.Length + methods.Length

    member _.GetSignature(Index i: RawIndex<MethodLocalVariables>) = &locals.ItemRef(i - 1)
    member _.GetSignature(Index i: RawIndex<FunctionPointer>) = &methods.ItemRef(i - 1)

    member internal _.RowIndex(Index i: RawIndex<MethodLocalVariables>) = uint32 i
    member internal _.RowIndex(Index i: RawIndex<FunctionPointer>) = uint32 i + uint32 locals.Length

    interface IMetadataTable<MethodLocalVariables> with
        member this.Count = this.TotalCount
        member this.Item with get i = &locals'.ItemRef(this.GetSignature(i))

[<Sealed>]
type StandAloneSigTableBuilder internal () =
    let locals = ImmutableArray.CreateBuilder<Blob<MethodLocalVariables>>()
    let methods = ImmutableArray.CreateBuilder<FunctionPointer>()

    member _.AddLocals variables =
        locals.Add variables
        RawIndex<MethodLocalVariables> locals.Count

    member _.AddSignature(signature: FunctionPointer) =
        methods.Add signature
        RawIndex<FunctionPointer> methods.Count

    member internal _.ToImmutable(locals') =
        StandAloneSigTable(locals.ToImmutable(), methods.ToImmutable(), locals')
