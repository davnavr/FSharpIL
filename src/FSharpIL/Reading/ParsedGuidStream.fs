﻿namespace FSharpIL.Reading

open System

open Microsoft.FSharp.Core.Operators.Checked

open FSharpIL

/// <summary>Represents an index into the <c>#GUID</c> metadata heap (II.24.2.5).</summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ParsedGuid (offset: uint32) =
    member _.Offset = offset
    member _.IsNull = offset = 0u

[<AutoOpen>]
module ParsedGuid = let inline (|ParsedGuid|) (index: ParsedGuid) = index.Offset

[<NoComparison; NoEquality>]
type ParsedGuidStream =
    internal
        { Chunk: ChunkReader
          GuidOffset: uint64
          GuidSize: uint64 }
    member this.IsValidOffset offset = this.Chunk.IsValidOffset(uint64 offset + 15UL)
    member this.TryGetGuid(ParsedGuid i) =
        match i with
        | 0u -> Ok Guid.Empty
        | _ when this.IsValidOffset i ->
            let buffer = Span.stackalloc<byte> 16
            this.Chunk.ReadBytes(this.GuidOffset + (uint64 i * 16UL), buffer)
            Ok(Guid(Span.asReadOnly buffer))
        | _ -> Error(invalidOp "error")
