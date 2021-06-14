namespace FSharpIL.Writing

open System
open System.Collections.Generic

open FSharpIL.Utilities
open FSharpIL.Utilities.Collections

open FSharpIL
open FSharpIL.Metadata

/// <summary>Builds the <c>#Strings</c> metadata stream, containing null-terminated UTF-8 strings (II.24.2.3).</summary>
[<Sealed>]
type StringsStreamBuilder (capacity: int32) =
    static let empty = ReadOnlyMemory.Empty
    let mutable offset = { StringOffset = 1u }
    // NOTE: Avoid struct copying by somehow getting inref to values.
    let strings = RefArrayList<ReadOnlyMemory<char>> capacity
    let lookup = Dictionary<ReadOnlyMemory<char>, StringOffset>(capacity, StringLookupComparer.Instance)
    do strings.Add &empty |> ignore // First entry is the empty string.
    do lookup.[ReadOnlyMemory.Empty] <- { StringOffset = 0u }

    member _.IsEmpty = strings.Count = 1

    /// <summary>The length of the <c>#Strings</c> metadata stream, in bytes.</summary>
    member _.StreamLength = offset.StringOffset

    member private _.AddUnsafe(str: inref<ReadOnlyMemory<_>>) =
        let offset' = offset
        offset <- { StringOffset = offset.StringOffset + 1u + uint32 str.Length } |> noImpl "calculate length correctly"
        lookup.[str] <- offset'
        strings.Add &str |> ignore
        offset'

    // TODO: Better parameter validation to prevent null characters in string, maybe return ValueNone?
    member private this.GetOrAdd str =
        match lookup.TryGetValue str with
        | true, existing -> existing
        | false, _ -> this.AddUnsafe &str

    member private this.GetOrAddFolded str =
        match lookup.TryGetValue str with
        | true, existing -> existing
        | false, _ ->
            for i = 1 to str.Length - 1 do
                lookup.[str.Slice i] <- { StringOffset = offset.StringOffset + uint32 i }
            this.AddUnsafe &str

    member this.GetOrAdd(str: Identifier) = this.GetOrAdd(Identifier.asMemory str)
    member this.GetOrAddFolded(str: Identifier) = this.GetOrAddFolded(Identifier.asMemory str)

    interface IStreamBuilder with
        member this.StreamLength = ValueSome this.StreamLength
        member _.StreamName = Magic.StreamNames.strings
        member _.Serialize builder =
            let mutable chars = ReadOnlySpan<char>()
            let mutable buffer = Span.stackalloc<byte> 512
            for i = 0 to strings.Count - 1 do do
                let str = &strings.[i]
                chars <- str.Span
                while chars.Length > 0 do
                    let length = min buffer.Length chars.Length
                    System.Text.Encoding.UTF8.GetBytes(chars, buffer) |> ignore // TODO: Use Encoder.GetBytes and Encoder.Reset, since strings are processed in chunks.
                    builder.Write(buffer.Slice(0, length))
                    chars <- chars.Slice length
                builder.Write 0uy
