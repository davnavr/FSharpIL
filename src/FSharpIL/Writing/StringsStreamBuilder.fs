namespace FSharpIL.Writing

open System
open System.Collections.Generic

open FSharpIL.Utilities

open FSharpIL
open FSharpIL.Metadata

/// <summary>Builds the <c>#Strings</c> metadata stream, containing null-terminated UTF-8 strings (II.24.2.3).</summary>
[<Sealed>]
type StringsStreamBuilder (capacity: int32) =
    let mutable offset = { StringOffset = 1u }
    // NOTE: Avoid struct copying by somehow getting inref to values.
    let strings = List<ReadOnlyMemory<char>> capacity
    let offsetLookup = Dictionary<ReadOnlyMemory<char>, StringOffset>(capacity, StringsStreamBuilder.StringComparer)
    let stringLookup = Dictionary<StringOffset, int32> capacity
    do strings.Add ReadOnlyMemory.Empty // First entry is the empty string.
    do offsetLookup.[ReadOnlyMemory.Empty] <- StringOffset.Zero
    do stringLookup.[StringOffset.Zero] <- 0

    new () = StringsStreamBuilder 1024

    static member val private StringComparer =
        { new IEqualityComparer<ReadOnlyMemory<char>> with
            member _.Equals(x, y) =
                if x.Length = y.Length then
                    let mutable equal, i = false, 0
                    while i < x.Length && not equal do
                        if x.Span.[i] = y.Span.[i] then equal <- true
                        i <- i + 1
                    equal
                else false
            member _.GetHashCode str =
                let hash = HashCode()
                for c in str.Span do hash.Add c
                hash.ToHashCode() }

    member _.IsEmpty = strings.Count = 1

    /// <summary>The length of the <c>#Strings</c> metadata stream, in bytes.</summary>
    member _.StreamLength = offset.StringOffset

    member private _.AddUnsafe(str: inref<ReadOnlyMemory<_>>) =
        let offset' = offset
        offset <- { StringOffset = offset.StringOffset + 1u + uint32 str.Length }
        offsetLookup.[str] <- offset'
        stringLookup.[offset'] <- strings.Count
        strings.Add str
        offset'

    // TODO: Better parameter validation to prevent null characters in string, maybe return ValueNone?
    member internal this.Add str =
        match offsetLookup.TryGetValue str with
        | true, existing -> existing
        | false, _ -> this.AddUnsafe &str

    member internal this.AddFolded str =
        match offsetLookup.TryGetValue str with
        | true, existing -> existing
        | false, _ ->
            for i = 1 to str.Length - 1 do
                offsetLookup.[str.Slice i] <- { StringOffset = offset.StringOffset + uint32 i }
            this.AddUnsafe &str

    member this.Add(str: Identifier) = this.Add(Identifier.asMemory str)
    member this.AddFolded(str: Identifier) = this.AddFolded(Identifier.asMemory str)

    member _.TryGetChars(offset, chars: outref<ReadOnlyMemory<char>>) =
        match stringLookup.TryGetValue offset with
        | true, i ->
            chars <- strings.[i]
            true
        | false, _ -> false

    member this.TryGetString(offset, str: outref<string>) =
        match this.TryGetChars offset with
        | true, chars ->
            str <- String chars.Span
            true
        | false, _ -> false

    interface IStreamBuilder with
        member this.StreamLength = this.StreamLength
        member _.StreamName = Magic.StreamNames.strings
        member _.Serialize builder =
            let mutable chars = ReadOnlySpan<char>()
            let mutable buffer = Span.stackalloc<byte> 512
            for str in strings do
                chars <- str.Span
                while chars.Length > 0 do
                    let length = min buffer.Length chars.Length
                    System.Text.Encoding.UTF8.GetBytes(chars, buffer) |> ignore
                    builder.Write(buffer.Slice(0, length))
                    chars <- chars.Slice length
                builder.Write 0uy
