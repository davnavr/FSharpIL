namespace FSharpIL.Reading

/// <summary>Represents an offset into the <c>#US</c> metadata heap (II.24.2.4).</summary>
[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type ParsedUserString (offset: uint32) =
    member _.Offset = offset
    member _.IsNull = offset = 0u

[<AutoOpen>]
module ParsedUserString = let (|ParsedUserString|) (offset: ParsedUserString) = offset.Offset

/// <summary>Represents the <c>#US</c> metadata heap, which contains UTF-16 strings (II.24.2.4).</summary>
[<Sealed>]
type ParsedUserStringStream internal (stream: ParsedMetadataStream) =
    member _.Size = stream.StreamSize
