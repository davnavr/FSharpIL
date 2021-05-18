namespace FSharpIL.Reading

open System

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL

/// <summary>Represents an index into the <c>#GUID</c> metadata heap (II.24.2.5).</summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ParsedGuid =
    internal { GuidIndex: uint32 }
    member this.IsZero = this.GuidIndex = 0u
    override this.ToString() = sprintf "0x%08X" this.GuidIndex
    static member op_Implicit { GuidIndex = offset } = offset

type ParsedGuidStream internal (chunk: ChunkedMemory) =
    member _.Count = chunk.Length / 16u

    member _.IsValidOffset offset = chunk.IsValidOffset(offset + 15u)

    member this.TryGetGuid { ParsedGuid.GuidIndex = i } =
        match i with
        | 0u -> ValueSome Guid.Empty
        | _ ->
            let buffer = Span.stackalloc<byte> sizeof<Guid>
            let offset = (i - 1u) * uint32 buffer.Length
            if chunk.TryCopyTo(offset, buffer)
            then ValueSome(Guid(Span.asReadOnly buffer))
            else ValueNone

    member this.GetGuid guid =
        match this.TryGetGuid guid with
        | ValueSome guid' -> guid'
        | ValueNone ->
            invalidArg "guid" (sprintf "Cannot find GUID %O, last valid guid is at index 0x%08X" guid (this.Count - 1u))
