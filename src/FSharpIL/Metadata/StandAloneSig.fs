﻿namespace FSharpIL.Metadata

open System.Collections.Immutable

[<Sealed>]
type StandAloneSigTable internal (locals: ImmutableArray<MethodLocalVariables>) =
    member _.LocalVariables = locals

    member _.TotalCount = locals.Length

    member _.GetSignature(Index i) = locals.[i - 1]

    interface IMetadataTable<MethodLocalVariables> with
        member _.Count = locals.Length
        member this.Item with get i = this.GetSignature i

[<Sealed>]
type StandAloneSigTableBuilder internal () =
    let locals = ImmutableArray.CreateBuilder<MethodLocalVariables>()

    member _.AddLocals variables = locals.Add variables; RawIndex<MethodLocalVariables> locals.Count

    member internal _.ToImmutable() = StandAloneSigTable(locals.ToImmutable())
