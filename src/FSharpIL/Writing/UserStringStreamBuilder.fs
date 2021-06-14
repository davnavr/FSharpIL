namespace FSharpIL.Writing

open System
open System.Collections.Generic

open FSharpIL.Utilities
open FSharpIL.Utilities.Collections

open FSharpIL.Metadata

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type UserStringEntry = { String: ReadOnlyMemory<char>; Length: uint32 }

// <summary>Builds the <c>#US</c> metadata stream, containing length prefixed UTF-16 strings (II.24.2.4).</summary>
[<Sealed>]
type UserStringStreamBuilder (capacity: int32) =
    static let encoding = System.Text.Encoding.Unicode
    static let empty = Unchecked.defaultof<UserStringEntry>
    let mutable offset = 1u
    let strings = RefArrayList<UserStringEntry> capacity
    let lookup = Dictionary<ReadOnlyMemory<char>, UserStringOffset>(capacity, StringLookupComparer.Instance)
    do strings.Add &empty |> ignore // First entry is empty blob.
    do lookup.[ReadOnlyMemory.Empty] <- { UserStringOffset = 0u }

    member _.IsEmpty = strings.Count = 1

    /// <summary>The length of the <c>#US</c> metadata stream, in bytes.</summary>
    member _.StreamLength = offset

    member _.GetOrAdd str =
        match lookup.TryGetValue str with
        | true, existing -> existing
        | false, _ ->
            let offset' = { UserStringOffset = offset } // TODO: Remove common code with #Strings metadata stream builder.
            let entry = { String = str; Length = uint32(encoding.GetByteCount str.Span) }
            offset <- offset + BlobWriter.compressedUnsignedSize entry.Length + entry.Length
            strings.Add &entry |> ignore
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
        member this.StreamLength = ValueSome this.StreamLength
        member _.StreamName = Magic.StreamNames.us
        member _.Serialize wr =
            let encoder = encoding.GetEncoder()
            let buffer = Span.stackalloc<byte> 512
            let mutable chars = ReadOnlySpan<char>()
            for i = 0 to strings.Count - 1 do
                let entry = &strings.[i]
                BlobWriter.compressedUnsigned entry.Length &wr // Append length before string
                chars <- entry.String.Span
                while chars.Length > 0 do
                    let length = min chars.Length buffer.Length
                    wr.Write(buffer.Slice(0, encoder.GetBytes(chars.Slice(0, length), buffer, (length = chars.Length))))
                    chars <- chars.Slice length
