namespace FSharpIL

open System
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Utilities

type internal IByteWriter = abstract Write: ReadOnlySpan<byte> -> unit

[<AbstractClass; Sealed; Extension>]
type internal ByteWriterExtensions =
    [<Extension>] static member inline Write(this: byref<#IByteWriter>, data: byte[]) = this.Write(ReadOnlySpan data)
    [<Extension>] static member inline Write(this: byref<#IByteWriter>, data: ImmutableArray<byte>) = this.Write(data.AsSpan())
    [<Extension>] static member inline Write(this: byref<#IByteWriter>, data: Span<byte>) = this.Write(Span.asReadOnly data)

    [<Extension>]
    static member WriteLE(this: byref<#IByteWriter>, value) =
        let span = Span.stackalloc<byte> 2
        this.Write(Span.asReadOnly(Bytes.ofU2 span value))

    [<Extension>]
    static member WriteLE(this: byref<#IByteWriter>, value) =
        let span = Span.stackalloc<byte> 4
        this.Write(Span.asReadOnly(Bytes.ofU4 span value))

    [<Extension>]
    static member WriteLE(this: byref<#IByteWriter>, value) =
        let span = Span.stackalloc<byte> 8
        this.Write(Span.asReadOnly(Bytes.ofU8 span value))
