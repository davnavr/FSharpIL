namespace FSharpIL.Writing

open System
open System.Collections.Generic

open FSharpIL.Metadata

open FSharpIL.Utilities.Collections

[<Struct>]
type private StringsStreamSerializer =
    interface StringHelpers.IStringSerializer<ReadOnlyMemory<char>> with
        member _.WriteBefore(_, _) = ()
        member _.GetChars str = &str
        member _.WriteAfter(_, wr) = wr.Write 0uy

/// <summary>Builds the <c>#Strings</c> metadata stream, containing null-terminated UTF-8 strings (II.24.2.3).</summary>
[<Sealed>]
type StringsStreamBuilder (capacity: int32) =
    static let empty = ReadOnlyMemory.Empty
    static let emptyi = { StringOffset = 0u }
    let mutable offset = { StringOffset = 1u }
    let strings = RefArrayList<ReadOnlyMemory<char>> capacity
    let lookup = Dictionary<ReadOnlyMemory<char>, StringOffset>(capacity, StringHelpers.comparer)
    do strings.Add &empty |> ignore // First entry is the empty string.
    do lookup.[empty] <- emptyi

    member _.IsEmpty = strings.Count = 1
    member _.EmptyString = emptyi

    /// <summary>The length of the <c>#Strings</c> metadata stream, in bytes.</summary>
    member _.StreamLength = offset.StringOffset

    member private _.AddUnsafe(str: inref<ReadOnlyMemory<_>>) =
        let offset' = offset
        offset <- { StringOffset = offset.StringOffset + 1u + uint32 str.Length }
        lookup.[str] <- offset'
        strings.Add &str |> ignore
        offset'

    member private this.Add str =
        match lookup.TryGetValue str with
        | true, existing -> existing
        | false, _ ->
            for i = 1 to str.Length - 1 do
                lookup.[str.Slice i] <- { StringOffset = offset.StringOffset + uint32 i }
            this.AddUnsafe &str

    member this.Add str = { IdentifierOffset.Offset = this.Add(Identifier.asMemory str) }
    member this.Add { FileName = name } = { FileNameOffset.Offset = this.Add name }

    member this.Add(str: Identifier voption) =
        match str with
        | ValueSome str' -> this.Add(str').Offset
        | ValueNone -> emptyi

    interface IStreamBuilder with
        member this.StreamLength = ValueSome this.StreamLength
        member _.StreamName = Magic.StreamNames.strings
        member _.Serialize wr = StringHelpers.serializeStringHeap<StringsStreamSerializer, _> System.Text.Encoding.UTF8 &wr strings
