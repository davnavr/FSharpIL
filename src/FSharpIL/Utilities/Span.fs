[<RequireQualifiedAccess>]
module internal FSharpIL.Utilities.Span

open System
open System.Runtime.CompilerServices

open Microsoft.FSharp.NativeInterop

#nowarn "9" // Uses of this construct may result in the generation of unverifiable .NET IL code.

/// <summary>Creates a <see cref="System.Span`1"/> from a region of memory allocated on the stack.</summary>
[<RequiresExplicitTypeArguments>]
let inline stackalloc<'T when 'T : unmanaged> length = Span<'T>(NativePtr.toVoidPtr(NativePtr.stackalloc<'T> length), length)

/// <summary>Creates a <see cref="System.Span`1"/> from an array allocated in the heap.</summary>
[<RequiresExplicitTypeArguments>]
let inline heapalloc<'T> length = Span<'T>(Array.zeroCreate<'T> length)

let inline asReadOnly span = Span<'T>.op_Implicit(span): ReadOnlySpan<'T>

let inline toBlock (span: Span<'T>) =
    let mutable data = Array.zeroCreate<'T> span.Length
    span.CopyTo(Span data)
    Unsafe.As<_, System.Collections.Immutable.ImmutableArray<'T>> &data

let readOnlyEqual (left: ReadOnlySpan<'T>) (right: ReadOnlySpan<'T>) =
    if left.Length = right.Length then
        let mutable equal, i = true, 0
        while equal && i < left.Length do
            if left.[i] <> right.[i]
            then equal <- false
            else i <- i + 1
        equal
    else false

let inline fromEnd source endi length = Span<'T>(source, endi - length + 1, length)
