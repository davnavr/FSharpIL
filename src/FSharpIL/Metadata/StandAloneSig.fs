namespace FSharpIL.Metadata

open System.Collections.Immutable

[<Sealed>]
type StandAloneSigTable internal (locals: ImmutableArray<Blob<MethodLocalVariables>>) =
    member _.LocalVariables = locals

    member _.TotalCount = locals.Length

    member _.GetSignature(Index i) = &locals.ItemRef(i - 1)

    interface IMetadataTable<Blob<MethodLocalVariables>> with
        member _.Count = locals.Length
        member this.Item with get i = &this.GetSignature(i)

[<Sealed>]
type StandAloneSigTableBuilder internal () =
    let locals = ImmutableArray.CreateBuilder<Blob<MethodLocalVariables>>()

    member _.AddLocals variables = locals.Add variables; RawIndex<MethodLocalVariables> locals.Count

    member internal _.ToImmutable() = StandAloneSigTable(locals.ToImmutable())
