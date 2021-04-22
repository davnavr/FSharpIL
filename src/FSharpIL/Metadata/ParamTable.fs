namespace FSharpIL.Metadata

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type internal ParamTableLookup internal (method: RawIndex<MethodDefRow>, i: int32) =
    member _.Method = method
    member _.Index = i

[<Sealed>]
type ParamTable internal
    (
        imap: Dictionary<RawIndex<ParamRow>, int32>,
        lookup: Dictionary<RawIndex<MethodDefRow>, ImmutableArray<RawIndex<ParamRow>>>,
        rows: ImmutableArray<ParamRow>
    ) =
    member _.Count = rows.Length
    member _.Rows = rows
    member _.Item with get i = &rows.ItemRef imap.[i]

    member _.GetParameters method =
        match lookup.TryGetValue method with
        | true, existing -> existing
        | false, _ -> ImmutableArray.Empty

    interface IMetadataTable<ParamRow> with
        member this.Count = this.Count
        member this.Item with get i = &this.[i]

[<Sealed>]
type ParamTableBuilder internal () =
    let lookup = Dictionary<RawIndex<MethodDefRow>, ImmutableArray<RawIndex<ParamRow>>>()
    let imap = Dictionary<RawIndex<ParamRow>, ParamRow>()

    member _.Count = imap.Count

    member _.TryAdd(method, parameters: ImmutableArray<ParamRow>) =
        match lookup.TryGetValue method with
        | true, _ -> ValueNone
        | false, _ ->
            let mutable indices = Array.zeroCreate<RawIndex<ParamRow>> parameters.Length
            for i = 0 to parameters.Length - 1 do
                let index = RawIndex<ParamRow>(imap.Count + 1)
                indices.[i] <- index
                imap.[index] <- parameters.[i]
            let indices' = Unsafe.As &indices
            lookup.[method] <- indices'
            ValueSome indices'

    member internal _.ToImmutable(methods: ImmutableArray<RawIndex<MethodDefRow>>) =
        let rows = ImmutableArray.CreateBuilder<ParamRow> imap.Count
        let imap' = Dictionary imap.Count
        for i = 0 to methods.Length - 1 do
            match lookup.TryGetValue methods.[i] with
            | true, parameters ->
                for row in parameters do
                    imap'.[row] <- rows.Count
                    rows.Add imap.[row]
            | false, _ -> ()
        ParamTable(imap', Dictionary lookup, rows.ToImmutable())

/// <summary>
/// Error used when there are duplicate rows in the <c>Param</c> table.
/// </summary>
/// <category>Errors</category>
[<Sealed>]
type DuplicateParamError (method: RawIndex<MethodDefRow>) =
    inherit ValidationError()
    member _.Method = method
