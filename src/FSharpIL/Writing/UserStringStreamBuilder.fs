namespace FSharpIL.Writing

open System
open System.Collections.Generic

open FSharpIL.Utilities

open FSharpIL.Metadata

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type UserStringEntry = { String: ReadOnlyMemory<char>; Length: uint32 }

// <summary>Builds the <c>#US</c> metadata stream, containing length prefixed UTF-16 strings (II.24.2.4).</summary>
[<Sealed>]
type UserStringStreamBuilder (capacity: int32) =
    static let encoding = System.Text.Encoding.Unicode
    let mutable offset = 1u
    let strings = List<UserStringEntry> capacity
    let lookup = Dictionary<ReadOnlyMemory<char>, UserStringOffset>(capacity, StringLookupComparer.Instance)
    do strings.Add Unchecked.defaultof<_> // First entry is empty blob.
    do lookup.[ReadOnlyMemory.Empty] <- { UserStringOffset = 0u }

    member _.IsEmpty = strings.Count = 1

    /// <summary>The length of the <c>#US</c> metadata stream, in bytes.</summary>
    member _.StreamLength = offset |> noImpl "bad, does not include length of strings"

    member _.GetOrAdd str =
        match lookup.TryGetValue str with
        | true, existing -> existing
        | false, _ ->
            let offset' = { UserStringOffset = offset } // TODO: Remove common code with #Strings metadata stream builder.
            let entry = { String = str; Length = uint32(encoding.GetByteCount str.Span) }
            offset <- offset + BlobWriter.compressedUnsignedSize entry.Length + entry.Length
            strings.Add entry
            lookup.[str] <- offset'
            offset'

    member this.GetOrAddFolded str =
        let mutable offset' = offset
        let result = this.GetOrAdd str
        for i = 1 to str.Length - 1 do
            offset' <- offset' + 1u
            lookup.[str.Slice i] <- { UserStringOffset = offset' }
        result

    interface IStreamBuilder with
        member this.StreamLength = this.StreamLength
        member _.StreamName = Magic.StreamNames.us
        member _.Serialize wr =
            let encoder = encoding.GetEncoder()
            let buffer = Span.stackalloc<byte> 512
            let mutable chars = ReadOnlySpan<char>()
            for { String = str; Length = len } in strings do
                BlobWriter.compressedUnsigned len &wr // Append length before string
                chars <- str.Span
                while chars.Length > 0 do
                    let length = min chars.Length buffer.Length
                    wr.Write(buffer.Slice(0, encoder.GetBytes(chars.Slice(0, length), buffer, (length = chars.Length))))
                    chars <- chars.Slice length
