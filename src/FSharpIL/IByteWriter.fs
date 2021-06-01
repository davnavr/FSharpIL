namespace FSharpIL

type internal IByteWriter = abstract Write: System.ReadOnlySpan<byte> -> unit
