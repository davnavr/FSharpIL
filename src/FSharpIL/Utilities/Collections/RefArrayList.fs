namespace FSharpIL.Utilities.Collections

open System

open FSharpIL.Utilities

[<Sealed>]
type RefArrayList<'T when 'T : struct> internal (capacity) =
    static let [<Literal>] DefaultCapacity = 4
    let mutable items, i = Array.zeroCreate<'T> capacity, 0
    new () = RefArrayList DefaultCapacity
    member _.Count = i
    /// <summary>Gets a reference to an item in the list.</summary>
    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="index"/> is negative or is equal to or greater than the number of items in the list.
    /// </exception>
    member _.Item with get index: inref<'T> =
        if index < 0 then argOutOfRange "index" index "The index must not be negative"
        if index >= i then argOutOfRange "index" index "The index must not equal or exceed the length of the list"
        &items.[index]
    member internal _.Add(item: inref<'T>): inref<'T> =
        let i' = i
        if i' >= items.Length then System.Array.Resize(&items, items.Length * 2)
        items.[i'] <- item
        i <- i + 1
        &items.[i']
    member internal this.Add(item: 'T) = &this.Add &item
    member internal _.AsSpan() = ReadOnlySpan<'T>(items, 0, i)
    member internal _.AsMemory() = ReadOnlyMemory<'T>(items, 0, i)
