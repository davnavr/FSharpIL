namespace FSharpIL.Reading

open System

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL

/// <summary>Represents an index into the <c>#GUID</c> metadata heap (II.24.2.5).</summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ParsedGuid =
    internal { GuidOffset: uint32 }
    member this.IsZero = this.GuidOffset = 0u
    override this.ToString() = sprintf "0x%08X" this.GuidOffset
    static member op_Implicit { GuidOffset = offset } = offset

type ParsedGuidStream internal (chunk: ChunkedMemory) =
    member _.Count = chunk.Length / 16u

    member _.IsValidOffset offset = chunk.IsValidOffset(offset + 15u)

    member this.TryGetGuid { ParsedGuid.GuidOffset = i } =
        match i with
        | 0u -> Ok Guid.Empty
        | _ when this.IsValidOffset i ->
            let buffer = Span.stackalloc<byte> 16
            chunk.CopyTo(i * 16u, buffer)
            Ok(Guid(Span.asReadOnly buffer))
        | _ -> Error(invalidOp "TODO: error type for missing guid")

    member this.GetGuid guid =
        match this.TryGetGuid guid with
        | Ok guid' -> guid'
