[<RequireQualifiedAccess>]
module internal FSharpIL.Span

open System

open Microsoft.FSharp.NativeInterop

#nowarn "9"

/// <summary>Creates a <see cref="System.Span`1"/> from a region of memory allocated on the stack.</summary>
[<RequiresExplicitTypeArguments>]
let inline stackalloc<'T when 'T : unmanaged> length = Span<'T>(NativePtr.toVoidPtr(NativePtr.stackalloc<'T> length), length)

/// <summary>Creates a <see cref="System.Span`1"/> from an array allocated in the heap.</summary>
[<RequiresExplicitTypeArguments>]
let inline heapalloc<'T> length = Span<'T>(Array.zeroCreate<'T> length)
