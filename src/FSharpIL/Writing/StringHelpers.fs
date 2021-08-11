[<RequireQualifiedAccess>]
module internal FSharpIL.Writing.StringHelpers

open System

open FSharpIL
open FSharpIL.Utilities

let comparer =
    { new System.Collections.Generic.IEqualityComparer<ReadOnlyMemory<char>> with
        member _.Equals(x, y) = MemoryExtensions.Equals(x.Span, y.Span, StringComparison.Ordinal)
        member _.GetHashCode str = str.GetHashCode() }

type IStringSerializer<'String when 'String : struct> = interface
    abstract WriteBefore: inref<'String> * byref<ChunkedMemoryBuilder> -> unit
    abstract GetChars: inref<'String> -> inref<ReadOnlyMemory<char>>
    abstract WriteAfter: inref<'String> * byref<ChunkedMemoryBuilder> -> unit
end

let serializeStringHeap<'Serializer, 'String when 'Serializer :> IStringSerializer<'String> and 'Serializer : struct>
    (encoding: System.Text.Encoding)
    (wr: byref<ChunkedMemoryBuilder>)
    (strings: System.Collections.Immutable.ImmutableArray<'String>.Builder)
    =
    let encoder = encoding.GetEncoder()
    let buffer = Span.stackalloc<byte> 256
    let mutable chars = ReadOnlySpan<char>()
    for i = 0 to strings.Count - 1 do
        let string = &strings.ItemRef i
        Unchecked.defaultof<'Serializer>.WriteBefore(&string, &wr)

        chars <- Unchecked.defaultof<'Serializer>.GetChars(&string).Span

        if chars.Length > 0 then
            let mutable charsUsed, bytesUsed, completed = 0, 0, false

            while not completed || charsUsed > 0 do
                encoder.Convert(chars, buffer, chars.IsEmpty, &charsUsed, &bytesUsed, completed = &completed)
                wr.Write(buffer.Slice(0, bytesUsed))
                chars <- chars.Slice charsUsed

        Unchecked.defaultof<'Serializer>.WriteAfter(&string, &wr)
