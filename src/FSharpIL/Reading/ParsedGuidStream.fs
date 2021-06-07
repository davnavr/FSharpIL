namespace FSharpIL.Reading

open System

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ParsedGuidStream (chunk: ChunkedMemory) =
    member _.Count = chunk.Length / 16u

    member _.IsValidOffset offset = chunk.IsValidOffset(offset + 15u)

    member _.TryGetGuid index =
        match index with
        | { GuidIndex = 0u } -> Ok Guid.Empty
        | { GuidIndex = i } ->
            let buffer = Span.stackalloc<byte> sizeof<Guid>
            let offset = (i - 1u) * uint32 buffer.Length
            if chunk.TryCopyTo(offset, buffer)
            then Ok(Guid(Span.asReadOnly buffer))
            else Error(InvalidGuidIndex(index, { GuidIndex = chunk.Length / uint32 sizeof<Guid> }))
