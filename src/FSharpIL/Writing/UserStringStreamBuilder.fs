namespace FSharpIL.Writing

open System
open System.Collections.Generic
open System.Collections.Immutable

open FSharpIL.Metadata

open FSharpIL.Utilities.Collections

[<System.Runtime.CompilerServices.IsReadOnly; Struct>]
type private UserStringEntry =
    { String: ReadOnlyMemory<char>
      /// The number of bytes allocated to hold the string, excluding the terminal byte.
      Length: uint32
      TerminalByte: uint8 }

    member inline this.TotalLength = this.Length + 1u

[<AutoOpen>]
module private UserStringHelpers =
    let inline (|IsNonZero|) value = value = 0us
    let inline (|IsInRange|) min max value = value >= max && value <= min

    /// Determines the terminal byte of a string (II.24.2.4).
    let getTerminalByte (str: inref<ReadOnlyMemory<char>>) =
        let chars = str.Span
        let mutable i, terminal = 0, 0uy

        while i < str.Length && terminal = 0uy do
            let ch = uint16 chars.[i]
            match ch >>> 8, ch &&& 0xFFus with
            | IsNonZero true, _
            | _, IsInRange 0x1us 0x8us true
            | _, IsInRange 0xEus 0x1Fus true
            | _, 0x27us
            | _, 0x2Dus
            | _, 0x7Fus -> terminal <- 1uy
            | _ -> ()

            i <- i + 1
        terminal

[<Struct>]
type private UserStringSerializer =
    interface StringHelpers.IStringSerializer<UserStringEntry> with
        /// Appends the length of the blob before the string.
        member _.WriteBefore(entry, wr) = if entry.String.Length > 0 then BlobWriter.compressedUnsigned entry.TotalLength &wr
        member _.GetChars entry = &entry.String
        member _.WriteAfter(entry, wr) = wr.Write entry.TerminalByte

// <summary>Builds the <c>#US</c> metadata stream, containing length prefixed UTF-16 strings (II.24.2.4).</summary>
[<Sealed>]
type UserStringStreamBuilder (capacity: int32) =
    static let encoding = System.Text.Encoding.Unicode
    static let empty = Unchecked.defaultof<UserStringEntry>
    let mutable offset = 1u
    let strings = RefArrayList<UserStringEntry> capacity
    let lookup = Dictionary<ReadOnlyMemory<char>, UserStringOffset>(capacity, StringHelpers.comparer)
    do strings.Add &empty |> ignore // First entry is empty blob.
    do lookup.[ReadOnlyMemory.Empty] <- { UserStringOffset = 0u }

    member _.IsEmpty = strings.Count = 1

    /// <summary>The length of the <c>#US</c> metadata stream, in bytes.</summary>
    member _.StreamLength = offset

    member _.Add str =
        match lookup.TryGetValue str with
        | true, existing -> existing
        | false, _ ->
            let offset' = { UserStringOffset = offset } // TODO: Remove common code with #Strings metadata stream builder.
            let entry =
                { String = str
                  Length = uint32(encoding.GetByteCount str.Span)
                  TerminalByte = getTerminalByte &str }
            offset <- offset + BlobWriter.compressedUnsignedSize entry.TotalLength + entry.TotalLength
            strings.Add &entry |> ignore
            lookup.[str] <- offset'
            offset'

    member this.AddFolded str =
        let mutable offset' = offset
        let result = this.Add str
        for i = 1 to str.Length - 1 do
            offset' <- offset' + 1u
            lookup.[str.Slice i] <- { UserStringOffset = offset' }
        result

    member this.AddFolded(str: string) =
        match str with
        | null
        | "" -> Unchecked.defaultof<_>
        | _ -> this.AddFolded(str.AsMemory())

    interface IStreamBuilder with
        member this.StreamLength = ValueSome this.StreamLength
        member _.StreamName = Magic.StreamNames.us
        member _.Serialize(wr, _) = StringHelpers.serializeStringHeap<UserStringSerializer, _> encoding &wr strings
